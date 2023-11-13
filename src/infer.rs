use core::fmt;
use std::collections::{BTreeMap, HashMap};

use crate::expr::{Expr, Id, Level, Pattern, Type, TypeVar};

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeVarNotFound(Id),
    RecursiveType,
    CannotUnify(String, String),
    MissingLabel(String),
    NotARow(String),
    RecursiveRowType,
    UnexpectedNumberOfArguments,
    ExpectedFunction,
    VariableNotFound(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::TypeVarNotFound(id) => write!(f, "type var not found: {}", id),
            Error::RecursiveType => write!(f, "recursive type"),
            Error::CannotUnify(ty1, ty2) => {
                write!(f, "cannot unify\n  '{}'\n  '{}", ty1, ty2)
            }
            Error::MissingLabel(label) => write!(f, "missing label: {}", label),
            Error::NotARow(ty) => {
                write!(f, "not a row: {}", ty)
            }
            Error::RecursiveRowType => write!(f, "recursive row type"),
            Error::UnexpectedNumberOfArguments => write!(f, "unexpected number of arguments"),
            Error::ExpectedFunction => write!(f, "expected a function"),
            Error::VariableNotFound(var) => write!(f, "variable not found: {}", var),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

trait BTreeMapExt<K: std::cmp::Ord, V> {
    fn singleton(k: K, v: V) -> BTreeMap<K, V>;
}

impl<K: std::cmp::Ord, V> BTreeMapExt<K, V> for BTreeMap<K, V> {
    fn singleton(k: K, v: V) -> BTreeMap<K, V> {
        let mut map = BTreeMap::new();
        map.insert(k, v);
        map
    }
}

#[derive(Clone, Default)]
pub struct Env {
    pub vars: HashMap<String, Type>,
    pub type_vars: Vec<TypeVar>,
}

impl Env {
    fn new_unbound(&mut self, level: Level) -> Type {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar::Unbound(level));
        Type::Var(id)
    }

    fn new_generic(&mut self) -> Type {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar::Generic);
        Type::Var(id)
    }

    fn get_type_var(&self, id: Id) -> Result<&TypeVar> {
        self.type_vars.get(id).ok_or(Error::TypeVarNotFound(id))
    }

    fn get_mut_type_var(&mut self, id: Id) -> Result<&mut TypeVar> {
        self.type_vars.get_mut(id).ok_or(Error::TypeVarNotFound(id))
    }

    fn link(&mut self, id: Id, ty: Type) -> Result<()> {
        let type_var = self.get_mut_type_var(id)?;
        *type_var = TypeVar::Link(ty);
        Ok(())
    }

    fn occurs_check_adjust_levels(&mut self, id: Id, level: Level, ty: &Type) -> Result<()> {
        match ty {
            Type::Var(other_id) => {
                let other = self.get_mut_type_var(*other_id)?;
                match other.clone() {
                    TypeVar::Link(ty) => self.occurs_check_adjust_levels(id, level, &ty),
                    TypeVar::Generic => panic!(),
                    TypeVar::Unbound(other_level) => {
                        if other_id == &id {
                            return Err(Error::RecursiveType);
                        } else if other_level > level {
                            *other = TypeVar::Unbound(level);
                        }
                        Ok(())
                    }
                }
            }
            Type::App(ty, args) => {
                for arg in args {
                    self.occurs_check_adjust_levels(id, level, arg)?;
                }
                self.occurs_check_adjust_levels(id, level, ty)
            }
            Type::Arrow(params, ret) => {
                for param in params {
                    self.occurs_check_adjust_levels(id, level, param)?;
                }
                self.occurs_check_adjust_levels(id, level, ret)
            }
            Type::Record(row) => self.occurs_check_adjust_levels(id, level, row),
            Type::Variant(row) => self.occurs_check_adjust_levels(id, level, row),
            Type::RowExtend(labels, rest) => {
                for ty in labels.values() {
                    self.occurs_check_adjust_levels(id, level, ty)?;
                }
                self.occurs_check_adjust_levels(id, level, rest)
            }
            Type::Const(_) | Type::RowEmpty => Ok(()),
        }
    }

    fn cannot_unify(&self, ty1: &Type, ty2: &Type) -> Result<()> {
        let ty1 = self.ty_to_string(ty1)?;
        let ty2 = self.ty_to_string(ty2)?;
        Err(Error::CannotUnify(ty1, ty2))
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        if ty1 == ty2 {
            return Ok(());
        }
        match (ty1, ty2) {
            (Type::Const(name1), Type::Const(name2)) if name1 == name2 => Ok(()),
            (Type::App(app_ty1, args1), Type::App(app_ty2, args2)) => {
                if args1.len() != args2.len() {
                    return self.cannot_unify(ty1, ty2);
                }
                for i in 0..args1.len() {
                    let arg1 = &args1[i];
                    let arg2 = &args2[i];
                    self.unify(arg1, arg2)?;
                }
                self.unify(app_ty1, app_ty2)
            }
            (Type::Arrow(params1, ret1), Type::Arrow(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return self.cannot_unify(ty1, ty2);
                }
                for i in 0..params1.len() {
                    let param1 = &params1[i];
                    let param2 = &params2[i];
                    self.unify(param1, param2)?;
                }
                self.unify(ret1, ret2)
            }
            (Type::Var(id1), Type::Var(id2)) if id1 == id2 => {
                panic!("multiple instance of a type variable")
            }
            (Type::Var(id), _) => {
                let tvar = self.get_type_var(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty2)?;
                        self.link(*id, ty2.clone())
                    }
                    TypeVar::Link(ty1) => self.unify(&ty1, ty2),
                    TypeVar::Generic => self.cannot_unify(ty1, ty2),
                }
            }
            (_, Type::Var(id)) => {
                let tvar = self.get_type_var(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty1)?;
                        self.link(*id, ty1.clone())
                    }
                    TypeVar::Link(ty2) => self.unify(ty1, &ty2),
                    TypeVar::Generic => self.cannot_unify(ty1, ty2),
                }
            }
            (Type::Record(row1), Type::Record(row2)) => self.unify(row1, row2),
            (Type::Variant(row1), Type::Variant(row2)) => self.unify(row1, row2),
            (Type::RowEmpty, Type::RowEmpty) => Ok(()),
            (Type::RowExtend(_, _), Type::RowExtend(_, _)) => self.unify_rows(ty1, ty2),
            (Type::RowEmpty, Type::RowExtend(labels, _))
            | (Type::RowExtend(labels, _), Type::RowEmpty) => {
                let (label, _) = labels.first_key_value().unwrap();
                Err(Error::MissingLabel(label.clone()))
            }
            _ => self.cannot_unify(ty1, ty2),
        }
    }

    fn not_a_row<T>(&self, ty: &Type) -> Result<T> {
        let ty = self.ty_to_string(ty)?;
        Err(Error::NotARow(ty))
    }

    fn match_row_ty(&self, ty: &Type) -> Result<(BTreeMap<String, Type>, Type)> {
        match ty {
            Type::RowExtend(labels, rest) => {
                let (mut rest_labels, rest) = self.match_row_ty(rest)?;
                for (label, ty) in labels {
                    rest_labels.insert(label.clone(), ty.clone());
                }
                Ok((rest_labels, rest))
            }
            Type::Var(id) => {
                let type_var = self.get_type_var(*id)?;
                match type_var.clone() {
                    TypeVar::Link(ty) => self.match_row_ty(&ty),
                    _ => Ok((BTreeMap::new(), ty.clone())),
                }
            }
            Type::RowEmpty => Ok((BTreeMap::new(), Type::RowEmpty)),
            _ => self.not_a_row(ty),
        }
    }

    fn find_missing(
        &mut self,
        labels1: &BTreeMap<String, Type>,
        labels2: &BTreeMap<String, Type>,
    ) -> Result<BTreeMap<String, Type>> {
        let mut missing = BTreeMap::new();
        for (label1, ty1) in labels1 {
            match labels2.get(label1) {
                Some(ty2) => {
                    self.unify(ty1, ty2)?;
                }
                None => {
                    missing.insert(label1.clone(), ty1.clone());
                }
            }
        }
        Ok(missing)
    }

    fn unify_rows(&mut self, row1: &Type, row2: &Type) -> Result<()> {
        let (labels1, rest1) = self.match_row_ty(row1)?;
        let (labels2, rest2) = self.match_row_ty(row2)?;

        let missing1 = self.find_missing(&labels2, &labels1)?;
        let missing2 = self.find_missing(&labels1, &labels2)?;

        match (missing1.is_empty(), missing2.is_empty()) {
            (true, true) => self.unify(&rest1, &rest2),
            (true, false) => self.unify(&rest2, &Type::RowExtend(missing2, rest1.into())),
            (false, true) => self.unify(&rest1, &Type::RowExtend(missing1, rest2.into())),
            (false, false) => match rest1 {
                Type::RowEmpty => {
                    let var = self.new_unbound(0);
                    self.unify(&rest1, &Type::RowExtend(missing1, var.into()))
                }
                Type::Var(id) => {
                    let type_var = self.get_type_var(id)?;
                    match type_var.clone() {
                        TypeVar::Unbound(level) => {
                            let rest = self.new_unbound(level);
                            self.unify(&rest2, &Type::RowExtend(missing2, rest.clone().into()))?;
                            if let TypeVar::Link(_) = self.get_type_var(id)? {
                                return Err(Error::RecursiveRowType);
                            }
                            self.unify(&rest1, &Type::RowExtend(missing1, rest.into()))
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
        }
    }

    fn generalize(&mut self, level: Level, ty: &Type) -> Result<()> {
        match ty {
            Type::Var(id) => {
                let type_var = self.get_mut_type_var(*id)?;
                match type_var.clone() {
                    TypeVar::Unbound(other_level) if other_level > level => {
                        *type_var = TypeVar::Generic;
                        Ok(())
                    }
                    TypeVar::Unbound(_) => Ok(()),
                    TypeVar::Link(ty) => self.generalize(level, &ty),
                    TypeVar::Generic => Ok(()),
                }
            }
            Type::App(ty, args) => {
                for arg in args {
                    self.generalize(level, arg)?;
                }
                self.generalize(level, ty)
            }
            Type::Arrow(params, ret) => {
                for param in params {
                    self.generalize(level, param)?;
                }
                self.generalize(level, ret)
            }
            Type::Record(row) => self.generalize(level, row),
            Type::Variant(row) => self.generalize(level, row),
            Type::RowExtend(labels, rest) => {
                for ty in labels.values() {
                    self.generalize(level, ty)?;
                }
                self.generalize(level, rest)
            }
            Type::Const(_) | Type::RowEmpty => Ok(()),
        }
    }

    fn instantiate(&mut self, level: Level, ty: Type) -> Result<Type> {
        let mut id_vars = HashMap::new();
        self.instantiate_inner(&mut id_vars, level, ty)
    }

    fn instantiate_inner(
        &mut self,
        id_vars: &mut HashMap<Id, Type>,
        level: Level,
        ty: Type,
    ) -> Result<Type> {
        match ty {
            Type::Const(_) => Ok(ty),
            Type::Var(id) => {
                let type_var = self.get_type_var(id)?;
                match type_var.clone() {
                    TypeVar::Unbound(_) => Ok(ty),
                    TypeVar::Link(ty) => self.instantiate_inner(id_vars, level, ty),
                    TypeVar::Generic => {
                        let ty = id_vars.entry(id).or_insert_with(|| self.new_unbound(level));
                        Ok(ty.clone())
                    }
                }
            }
            Type::App(ty, args) => {
                let instantiated_ty = self.instantiate_inner(id_vars, level, *ty)?;
                let mut instantiated_args = Vec::with_capacity(args.len());
                for arg in args {
                    let instantiated_arg = self.instantiate_inner(id_vars, level, arg)?;
                    instantiated_args.push(instantiated_arg);
                }
                Ok(Type::App(instantiated_ty.into(), instantiated_args))
            }
            Type::Arrow(params, ret) => {
                let instantiated_ret = self.instantiate_inner(id_vars, level, *ret)?;
                let mut instantiated_params = Vec::with_capacity(params.len());
                for param in params {
                    let instantiated_param = self.instantiate_inner(id_vars, level, param)?;
                    instantiated_params.push(instantiated_param);
                }
                Ok(Type::Arrow(instantiated_params, instantiated_ret.into()))
            }
            Type::Record(row) => Ok(Type::Record(
                self.instantiate_inner(id_vars, level, *row)?.into(),
            )),
            Type::Variant(row) => Ok(Type::Variant(
                self.instantiate_inner(id_vars, level, *row)?.into(),
            )),
            Type::RowEmpty => Ok(ty),
            Type::RowExtend(labels, rest) => {
                let mut instantiated_labels = BTreeMap::new();
                for (label, ty) in labels {
                    let instantiated_ty = self.instantiate_inner(id_vars, level, ty)?;
                    instantiated_labels.insert(label, instantiated_ty);
                }
                let rest = self.instantiate_inner(id_vars, level, *rest)?;
                Ok(Type::RowExtend(instantiated_labels, rest.into()))
            }
        }
    }

    fn match_fun_ty(&mut self, num_params: usize, ty: Type) -> Result<(Vec<Type>, Box<Type>)> {
        match ty {
            Type::Arrow(params, ret) => {
                if params.len() != num_params {
                    Err(Error::UnexpectedNumberOfArguments)
                } else {
                    Ok((params, ret))
                }
            }
            Type::Var(id) => {
                let type_var = self.get_type_var(id)?;
                match type_var.clone() {
                    TypeVar::Unbound(level) => {
                        let params: Vec<_> =
                            (0..num_params).map(|_| self.new_unbound(level)).collect();
                        let ret: Box<_> = self.new_unbound(level).into();
                        let ty = Type::Arrow(params.clone(), ret.clone());
                        self.link(id, ty)?;
                        Ok((params, ret))
                    }
                    TypeVar::Link(ty) => self.match_fun_ty(num_params, ty),
                    TypeVar::Generic => Err(Error::ExpectedFunction),
                }
            }
            _ => Err(Error::ExpectedFunction),
        }
    }
}

impl Env {
    fn get_var(&self, name: &str) -> Result<&Type> {
        if let Some(ty) = self.vars.get(name) {
            return Ok(ty);
        }
        Err(Error::VariableNotFound(name.to_owned()))
    }

    pub fn insert_var(&mut self, name: String, ty: Type) {
        self.vars.insert(name, ty);
    }

    pub fn infer(&mut self, expr: &Expr) -> Result<Type> {
        let ty = self.infer_inner(0, expr)?;
        self.generalize(-1, &ty)?;
        Ok(ty)
    }

    fn infer_pattern(&mut self, pattern: &Pattern, ty: Type) -> Result<()> {
        match pattern {
            Pattern::Var(var) => {
                self.insert_var(var.clone(), ty);
                Ok(())
            }
            Pattern::Record(labels) => {
                let (labels_ty, _) = self.match_row_ty(&ty)?;
                for (label, label_pattern) in labels {
                    match labels_ty.get(label) {
                        None => return Err(Error::MissingLabel(label.clone())),
                        Some(label_ty) => self.infer_pattern(label_pattern, label_ty.clone())?,
                    }
                }
                Ok(())
            }
        }
    }

    fn infer_inner(&mut self, level: Level, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Bool(_) => Ok(Type::bool()),
            Expr::Int(_) => Ok(Type::int()),
            Expr::IntBinOp(_, lhs, rhs) => {
                let ty = Type::int();
                let lhs_ty = self.infer_inner(level, lhs)?;
                self.unify(&ty, &lhs_ty)?;
                let rhs_ty = self.infer_inner(level, rhs)?;
                self.unify(&ty, &rhs_ty)?;
                Ok(ty)
            }
            Expr::Negate(expr) => {
                let ty = Type::bool();
                let expr_ty = self.infer_inner(level, expr)?;
                self.unify(&ty, &expr_ty)?;
                Ok(ty)
            }
            Expr::EqualEqual(lhs, rhs) => {
                let lhs = self.infer_inner(level, lhs)?;
                let rhs = self.infer_inner(level, rhs)?;
                self.unify(&lhs, &rhs)?;
                Ok(lhs)
            }
            Expr::Var(name) => {
                let ty = self.get_var(name)?.clone();
                self.instantiate(level, ty)
            }
            Expr::Fun(params, body) => {
                let mut param_tys = Vec::with_capacity(params.len());
                let old_vars = self.vars.clone();
                for param in params {
                    let param_ty = self.new_unbound(level);
                    self.infer_pattern(param, param_ty.clone())?;
                    param_tys.push(param_ty);
                }
                let ret_ty = self.infer_inner(level, body)?;
                self.vars = old_vars;
                Ok(Type::Arrow(param_tys, ret_ty.into()))
            }
            Expr::Let(pattern, value, body) => {
                let var_ty = self.infer_inner(level + 1, value)?;
                self.generalize(level, &var_ty)?;
                self.infer_pattern(pattern, var_ty)?;
                self.infer_inner(level, body)
            }
            Expr::Call(f, args) => {
                let f_ty = self.infer_inner(level, f)?;
                let (params, ret) = self.match_fun_ty(args.len(), f_ty)?;
                for i in 0..args.len() {
                    let arg = &args[i];
                    let arg_ty = self.infer_inner(level, arg)?;
                    let param = &params[i];
                    self.unify(&arg_ty, param)?;
                }
                Ok(*ret)
            }
            Expr::RecordEmpty => Ok(Type::Record(Type::RowEmpty.into())),
            Expr::RecordSelect(record, label) => {
                let rest = self.new_unbound(level);
                let field = self.new_unbound(level);
                let param = Type::Record(
                    Type::RowExtend(
                        BTreeMap::singleton(label.clone(), field.clone()),
                        rest.into(),
                    )
                    .into(),
                );
                let ret = field;
                let record = self.infer_inner(level, record)?;
                self.unify(&param, &record)?;
                Ok(ret)
            }
            Expr::RecordRestrict(record, label) => {
                let rest = self.new_unbound(level);
                let field = self.new_unbound(level);
                let param = Type::Record(
                    Type::RowExtend(
                        BTreeMap::singleton(label.clone(), field),
                        rest.clone().into(),
                    )
                    .into(),
                );
                let ret = Type::Record(rest.into());
                let record = self.infer_inner(level, record)?;
                self.unify(&param, &record)?;
                Ok(ret)
            }
            Expr::RecordExtend(labels, record) => {
                let mut tys = BTreeMap::new();
                for (label, expr) in labels {
                    let ty = self.infer_inner(level, expr)?;
                    tys.insert(label.clone(), ty);
                }
                let rest = self.new_unbound(level);
                let record = self.infer_inner(level, record)?;
                self.unify(&Type::Record(rest.clone().into()), &record)?;
                Ok(Type::Record(Type::RowExtend(tys, rest.into()).into()))
            }
            Expr::Variant(label, expr) => {
                let rest = self.new_unbound(level);
                let variant = self.new_unbound(level);
                let param = variant.clone();
                let ret = Type::Variant(
                    Type::RowExtend(BTreeMap::singleton(label.clone(), variant), rest.into())
                        .into(),
                );
                let ty = &self.infer_inner(level, expr)?;
                self.unify(&param, ty)?;
                Ok(ret)
            }
            Expr::Case(expr, cases, None) => {
                let ret = self.new_unbound(level);
                let expr = self.infer_inner(level, expr)?;
                let cases_row = self.infer_cases(level, &ret, Type::RowEmpty, cases)?;
                self.unify(&expr, &Type::Variant(cases_row.into()))?;
                Ok(ret)
            }
            Expr::Case(expr, cases, Some((def_var, def_expr))) => {
                let def_variant = self.new_unbound(level);
                let old_vars = self.vars.clone();
                self.vars
                    .insert(def_var.clone(), Type::Variant(def_variant.clone().into()));
                let ret = self.infer_inner(level, def_expr)?;
                self.vars = old_vars;
                let expr = self.infer_inner(level, expr)?;
                let cases_row = self.infer_cases(level, &ret, def_variant, cases)?;
                self.unify(&expr, &Type::Variant(cases_row.into()))?;
                Ok(ret)
            }
        }
    }

    fn infer_cases(
        &mut self,
        level: Level,
        ret: &Type,
        rest: Type,
        cases: &Vec<(String, String, Expr)>,
    ) -> Result<Type> {
        let mut labels = BTreeMap::new();
        for (label, var, expr) in cases {
            let variant = self.new_unbound(level);
            let old_vars = self.vars.clone();
            self.vars.insert(var.clone(), variant.clone());
            let ty = self.infer_inner(level, expr)?;
            self.vars = old_vars;
            self.unify(ret, &ty)?;
            labels.insert(label.clone(), variant);
        }
        Ok(Type::RowExtend(labels, rest.into()))
    }

    pub fn replace_ty_constants_with_vars(&mut self, vars: Vec<String>, mut ty: Type) -> Type {
        if !vars.is_empty() {
            let env = vars
                .into_iter()
                .map(|name| (name, self.new_generic()))
                .collect();
            ty = ty.replace_const_with_vars(&env);
        }
        ty
    }

    pub fn ty_to_string(&self, ty: &Type) -> Result<String> {
        let mut namer = Namer::new();
        self.ty_to_string_impl(&mut namer, ty, false, false)
    }

    fn real_ty(&self, ty: Type) -> Result<Type> {
        match ty {
            Type::Var(id) => {
                let type_var = self.get_type_var(id)?;
                match type_var.clone() {
                    TypeVar::Link(ty) => self.real_ty(ty),
                    _ => Ok(ty),
                }
            }
            _ => Ok(ty),
        }
    }

    fn ty_to_string_impl(
        &self,
        namer: &mut Namer,
        ty: &Type,
        is_simple: bool,
        is_row: bool,
    ) -> Result<String> {
        match ty {
            Type::Const(name) => Ok(name.clone()),
            Type::App(ty, args) => {
                let mut ty_str = self.ty_to_string_impl(namer, ty, true, is_row)?;
                ty_str.push('[');
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        ty_str.push_str(", ")
                    }
                    let arg = self.ty_to_string_impl(namer, arg, false, is_row)?;
                    ty_str.push_str(&arg);
                }
                ty_str.push(']');
                Ok(ty_str)
            }
            Type::Arrow(params, ret) => {
                let mut ty_str = if is_simple {
                    "(".to_owned()
                } else {
                    "".to_owned()
                };
                if params.len() == 1 {
                    let param = self.ty_to_string_impl(namer, &params[0], true, is_row)?;
                    let ret = self.ty_to_string_impl(namer, ret, false, is_row)?;
                    ty_str.push_str(&param);
                    ty_str.push_str(" -> ");
                    ty_str.push_str(&ret);
                } else {
                    ty_str.push('(');
                    for (i, param) in params.iter().enumerate() {
                        if i != 0 {
                            ty_str.push_str(", ");
                        }
                        let param = self.ty_to_string_impl(namer, param, false, is_row)?;
                        ty_str.push_str(&param);
                    }
                    let ret = self.ty_to_string_impl(namer, ret, false, is_row)?;
                    ty_str.push_str(") -> ");
                    ty_str.push_str(&ret);
                }
                if is_simple {
                    ty_str.push(')');
                }
                Ok(ty_str)
            }
            Type::Var(id) => {
                let tvar = self.get_type_var(*id)?;
                match tvar {
                    TypeVar::Generic => {
                        if is_row {
                            let name = namer.get_or_insert_row_name(*id);
                            Ok(format!("r{}", name))
                        } else {
                            let name = namer.get_or_insert_name(*id);
                            Ok(name.to_string())
                        }
                    }
                    TypeVar::Unbound(_) => Ok(format!("_{}", id)),
                    TypeVar::Link(ty) => self.ty_to_string_impl(namer, ty, is_simple, is_row),
                }
            }
            Type::Record(row) => {
                let ty = self.ty_to_string_impl(namer, row, is_simple, true)?;
                Ok(format!("{{{}}}", ty))
            }
            Type::Variant(row) => {
                let ty = self.ty_to_string_impl(namer, row, is_simple, true)?;
                Ok(format!("[{}]", ty))
            }
            Type::RowEmpty => Ok("".to_string()),
            Type::RowExtend(_, _) => {
                let (labels, rest) = self.match_row_ty(ty)?;
                let mut output = String::new();
                let mut sep = "";
                for (label, ty) in labels {
                    output.push_str(sep);
                    output.push_str(&label);
                    output.push_str(": ");
                    let ty = self.ty_to_string_impl(namer, &ty, is_simple, is_row)?;
                    output.push_str(&ty);
                    sep = ", ";
                }
                match self.real_ty(rest)? {
                    Type::RowEmpty => (),
                    Type::RowExtend(_, _) => unreachable!(),
                    other_ty => {
                        output.push_str(" | ");
                        let other_ty =
                            self.ty_to_string_impl(namer, &other_ty, is_simple, is_row)?;
                        output.push_str(&other_ty);
                    }
                }
                Ok(output)
            }
        }
    }
}

struct Namer {
    next_name: u8,
    names: HashMap<Id, u8>,
    next_row_name: u8,
    row_names: HashMap<Id, u8>,
}

impl Namer {
    fn new() -> Self {
        let next_name = 97;
        let names = HashMap::new();
        let next_row_name = 97;
        let row_names = HashMap::new();
        Self {
            next_name,
            names,
            next_row_name,
            row_names,
        }
    }

    fn get_or_insert_name(&mut self, id: Id) -> char {
        let name = match self.names.get(&id) {
            Some(name) => *name,
            None => {
                let name = self.next_name;
                self.next_name += 1;
                self.names.insert(id, name);
                name
            }
        };
        name as char
    }

    fn get_or_insert_row_name(&mut self, id: Id) -> char {
        let row_name = match self.row_names.get(&id) {
            Some(row_name) => *row_name,
            None => {
                let row_name = self.next_row_name;
                self.next_row_name += 1;
                self.row_names.insert(id, row_name);
                row_name
            }
        };
        row_name as char
    }
}

#[cfg(test)]
mod tests {
    use crate::{core::make_env, parser::Parser};

    use super::Error;

    enum Expected {
        Pass(String),
        Fail(Error),
    }

    fn pass(sig: &str) -> Expected {
        Expected::Pass(sig.to_owned())
    }

    fn fail(e: Error) -> Expected {
        Expected::Fail(e)
    }

    fn run_cases(cases: Vec<(&str, Expected)>) {
        let env = make_env();
        for (expr_str, expected) in cases {
            match expected {
                Expected::Pass(expected) => {
                    let (vars, ty) = Parser::ty(&expected).unwrap();
                    let mut env = env.clone();
                    let expected = env.replace_ty_constants_with_vars(vars, ty);
                    let expr = Parser::expr(expr_str).unwrap();
                    let actual = env.infer_inner(0, &expr).unwrap();
                    env.generalize(-1, &actual).unwrap();
                    let expected = env.ty_to_string(&expected).unwrap();
                    let actual = env.ty_to_string(&actual).unwrap();
                    assert_eq!(expected, actual, "for {}", expr_str);
                }
                Expected::Fail(expected) => {
                    let mut env = env.clone();
                    let expr = Parser::expr(expr_str).unwrap();
                    let actual = env.infer_inner(0, &expr).unwrap_err();
                    assert_eq!(expected, actual, "for {}", expr_str);
                }
            }
        }
    }

    #[test]
    fn infer_base() {
        let test_cases: Vec<(&str, Expected)> = vec![
            ("id", pass("forall[a] a -> a")),
            ("one", pass("int")),
            ("x", fail(Error::VariableNotFound("x".to_owned()))),
            (
                "let x = x in x",
                fail(Error::VariableNotFound("x".to_owned())),
            ),
            ("let x = id in x", pass("forall[a] a -> a")),
            ("let x = fun(y) -> y in x", pass("forall[a] a -> a")),
            ("fun(x) -> x", pass("forall[a] a -> a")),
            ("fun(x) -> x", pass("forall[int] int -> int")),
            ("pair", pass("forall[a b] (a, b) -> pair[a, b]")),
            ("pair", pass("forall[z x] (x, z) -> pair[x, z]")),
            (
                "fun(x) -> let y = fun(z) -> z in y",
                pass("forall[a b] a -> b -> b"),
            ),
            (
                "let f = fun(x) -> x in let id = fun(y) -> y in eq(f, id)",
                pass("bool"),
            ),
            (
                "let f = fun(x) -> x in let id = fun(y) -> y in eq_curry(f)(id)",
                pass("bool"),
            ),
            ("let f = fun(x) -> x in eq(f, succ)", pass("bool")),
            ("let f = fun(x) -> x in eq_curry(f)(succ)", pass("bool")),
            (
                "let f = fun(x) -> x in pair(f(one), f(true))",
                pass("pair[int, bool]"),
            ),
            (
                "fun(f) -> pair(f(one), f(true))",
                fail(Error::CannotUnify("bool".to_owned(), "int".to_owned())),
            ),
            (
                "let f = fun(x, y) -> let a = eq(x, y) in eq(x, y) in f",
                pass("forall[a] (a, a) -> bool"),
            ),
            (
                "let f = fun(x, y) -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f",
                pass("forall[a] (a, a) -> bool"),
            ),
            ("id(id)", pass("forall[a] a -> a")),
            (
                "choose(fun(x, y) -> x, fun(x, y) -> y)",
                pass("forall[a] (a, a) -> a"),
            ),
            (
                "choose_curry(fun(x, y) -> x)(fun(x, y) -> y)",
                pass("forall[a] (a, a) -> a"),
            ),
            (
                "let x = id in let y = let z = x(id) in z in y",
                pass("forall[a] a -> a"),
            ),
            ("cons(id, nil)", pass("forall[a] list[a -> a]")),
            ("cons_curry(id)(nil)", pass("forall[a] list[a -> a]")),
            (
                "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2",
                pass("list[int -> int]"),
            ),
            (
                "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))",
                pass("list[int -> int]"),
            ),
            (
                "plus(one, true)",
                fail(Error::CannotUnify("bool".to_owned(), "int".to_owned())),
            ),
            ("plus(one)", fail(Error::UnexpectedNumberOfArguments)),
            ("fun(x) -> let y = x in y", pass("forall[a] a -> a")),
            (
                "fun(x) -> let y = let z = x(fun(x) -> x) in z in y",
                pass("forall[a b] ((a -> a) -> b) -> b"),
            ),
            (
                "fun(x) -> fun(y) -> let x = x(y) in x(y)",
                pass("forall[a b] (a -> a -> b) -> a -> b"),
            ),
            (
                "fun(x) -> let y = fun(z) -> x(z) in y",
                pass("forall[a b] (a -> b) -> a -> b"),
            ),
            (
                "fun(x) -> let y = fun(z) -> x in y",
                pass("forall[a b] a -> b -> a"),
            ),
            (
                "fun(x) -> fun(y) -> let x = x(y) in fun(x) -> y(x)",
                pass("forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b"),
            ),
            ("fun(x) -> let y = x in y(y)", fail(Error::RecursiveType)),
            (
                "fun(x) -> let y = fun(z) -> z in y(y)",
                pass("forall[a b] a -> b -> b"),
            ),
            ("fun(x) -> x(x)", fail(Error::RecursiveType)),
            ("one(id)", fail(Error::ExpectedFunction)),
            (
                "fun(f) -> let x = fun(g, y) -> let _ = g(y) in eq(f, g) in x",
                pass("forall[a b] (a -> b) -> (a -> b, a) -> bool"),
            ),
            (
                "let const = fun(x) -> fun(y) -> x in const",
                pass("forall[a b] a -> b -> a"),
            ),
            (
                "let apply = fun(f, x) -> f(x) in apply",
                pass("forall[a b] (a -> b, a) -> b"),
            ),
            (
                "let apply_curry = fun(f) -> fun(x) -> f(x) in apply_curry",
                pass("forall[a b] (a -> b) -> a -> b"),
            ),
        ];
        run_cases(test_cases)
    }

    #[test]
    fn infer_records() {
        let test_cases = vec![
            ("{}", pass("{}")),
            ("{}.x", fail(Error::MissingLabel("x".to_owned()))),
            ("{a = one}", pass("{a : int}")),
            ("{a = one, b = true}", pass("{a : int, b : bool}")),
            ("{b = true, a = one}", pass("{b : bool, a : int}")),
            ("{a = one, b = true}.a", pass("int")),
            ("{a = one, b = true}.b", pass("bool")),
            (
                "{a = one, b = true}.c",
                fail(Error::MissingLabel("c".to_owned())),
            ),
            ("{f = fun(x) -> x}", pass("forall[a] {f : a -> a}")),
            (
                "let r = {a = id, b = succ} in choose(r.a, r.b)",
                pass("int -> int"),
            ),
            (
                "let r = {a = id, b = fun(x) -> x} in choose(r.a, r.b)",
                pass("forall[a] a -> a"),
            ),
            (
                "choose({a = one}, {})",
                fail(Error::MissingLabel("a".to_owned())),
            ),
            (
                "{ x = zero | { y = one | {} } }",
                pass("{y : int, x : int}"),
            ),
            (
                "choose({ x = zero | { y = one | {} } }, {x = one, y = zero})",
                pass("{y : int, x : int}"),
            ),
            ("{}\\x", fail(Error::MissingLabel("x".to_owned()))),
            ("{x = one, y = zero} \\ x", pass("{y : int}")),
            ("{ x = true | {x = one}}", pass("{x : bool}")),
            ("let a = {} in {b = one | a}", pass("{b : int}")),
            ("let a = {x = one} in {x = true | a}.x", pass("bool")),
            (
                "let a = {x = one} in a.y",
                fail(Error::MissingLabel("y".to_owned())),
            ),
            ("let a = {x = one} in a \\ x", pass("{}")),
            (
                "let a = {x = one} in let b = {x = true | a} in b\\x.x",
                fail(Error::MissingLabel("x".to_owned())),
            ),
            (
                "fun(r) -> {x = one | r}",
                pass("forall[r] {r} -> {x : int | r}"),
            ),
            ("fun(r) -> r.x", pass("forall[r a] {x : a | r} -> a")),
            (
                "let get_x = fun(r) -> r.x in get_x({y = one, x = zero})",
                pass("int"),
            ),
            (
                "let get_x = fun(r) -> r.x in get_x({y = one, z = true})",
                fail(Error::MissingLabel("x".to_owned())),
            ),
            (
                "fun(r) -> choose({x = zero | r}, {x = one | {}})",
                pass("{} -> {x : int}"),
            ),
            (
                "fun(r) -> choose({x = zero | r}, {x = one})",
                pass("{} -> {x : int}"),
            ),
            (
                "fun(r) -> choose({x = zero | r}, {x = one | r})",
                pass("forall[r] {r} -> {x : int | r}"),
            ),
            (
                "fun(r) -> choose({x = zero | r}, {y = one | r})",
                fail(Error::RecursiveRowType),
            ),
            ("let f = fun(x) -> x.t(one) in f({t = succ})", pass("int")),
            ("let f = fun(x) -> x.t(one) in f({t = id})", pass("int")),
            (
                "let f = fun(r) -> let y = r.y in choose(r, {x = one}) in f",
                fail(Error::MissingLabel("y".to_owned())),
            ),
            (
                "fun(r) -> choose({x = zero | r}, {x = true | r})",
                fail(Error::CannotUnify("int".to_owned(), "bool".to_owned())),
            ),
            (
                "fun(r, s) -> choose({b = true, c = zero | r}, {b = false, c = one, d = half | s})",
                pass("forall[a] ({d : float | a}, {a}) -> {b : bool, c : int, d : float | a}"),
            ),
            (
                "fun(r) -> {x = r | r}",
                pass("forall[a] {a} -> {x : {a} | a}"),
            ),
        ];
        run_cases(test_cases);
    }

    #[test]
    fn infer_variant() {
        let test_cases = vec![
            (":X one", pass("forall[a] [X : int | a]")),
            (
                "choose(choose(:x one, :Y true), choose(:X half, :y nil))",
                pass("forall[a b] [X : float, Y : bool, x : int, y : list[a] | b]"),
            ),
            (
                "choose(:X one, :X true)",
                fail(Error::CannotUnify("int".to_owned(), "bool".to_owned())),
            ),
            (
                "choose(:X {x = one, y = false}, :Y {w = half})",
                pass("forall[a] [X : {x : int, y : bool}, Y : {w : float} | a]"),
            ),
            (
                concat!(
                    "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
                    "match e { :x i -> i | :Y y -> zero}"
                ),
                fail(Error::MissingLabel("X".to_owned())),
            ),
            (
                "fun(x, y) -> match x {:a i -> one | :b i -> zero | :c i -> y}",
                pass("forall [a b c] ([a : a, b : b, c : c], int) -> int"),
            ),
            (
                "fun(a) -> match a {:X i -> i | r -> one}",
                pass("forall[a] [X : int | a] -> int"),
            ),
            (
                concat!(
                    "let f = fun(m) -> match m {:y a -> one | :Y b -> zero | :z z -> zero} in ",
                    "fun(e) -> match e { :x i -> i | :X f -> one | r -> f(r)}"
                ),
                pass("forall[a b c d] [X : a, Y : b, x : int, y : c, z : d] -> int"),
            ),
            (
                concat!(
                    "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
                    "let f = fun(m) -> match m {:y a -> one | :Y b -> zero | :z z -> zero} in ",
                    "match e { :x i -> i | :X f -> one | r -> f(r)}"
                ),
                pass("int"),
            ),
            (
                "fun(e) -> match e { :X a -> plus(a.x, one) }",
                pass("forall[a] [X : {x : int | a}] -> int"),
            ),
        ];
        run_cases(test_cases);
    }
}
