use core::fmt;
use std::collections::{BTreeMap, BTreeSet, HashMap};

use itertools::Itertools;

use crate::{
    expr::{Constraints, Expr, Id, Level, Pattern, Type, TypeVar},
    parser::ForAll,
};

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
    CannotInjectConstraintsInto(String),
    RowConstraintFailed(String),
    RecordPatternNotRecord(String),
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
            Error::CannotInjectConstraintsInto(ty) => {
                write!(f, "cannot inject constraints into: {}", ty)
            }
            Error::RowConstraintFailed(label) => {
                write!(f, "row constraint failed for label: {}", label)
            }
            Error::RecordPatternNotRecord(ty) => write!(f, "record pattern not a record: {}", ty),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

trait BTreeSetExt<K: std::cmp::Ord> {
    fn singleton(k: K) -> BTreeSet<K>;
}

impl<K: std::cmp::Ord> BTreeSetExt<K> for BTreeSet<K> {
    fn singleton(k: K) -> BTreeSet<K> {
        let mut set = BTreeSet::new();
        set.insert(k);
        set
    }
}

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

    fn new_unbound_row(&mut self, level: Level, constraints: Constraints) -> Type {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar::UnboundRow(level, constraints));
        Type::Var(id)
    }

    fn new_generic(&mut self) -> Type {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar::Generic);
        Type::Var(id)
    }

    fn new_generic_row(&mut self, constraints: Constraints) -> Type {
        let id = self.type_vars.len();
        self.type_vars.push(TypeVar::GenericRow(constraints));
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
                    TypeVar::Generic | TypeVar::GenericRow(_) => panic!(),
                    TypeVar::Unbound(other_level) => {
                        if other_id == &id {
                            return Err(Error::RecursiveType);
                        } else if other_level > level {
                            *other = TypeVar::Unbound(level);
                        }
                        Ok(())
                    }
                    TypeVar::UnboundRow(other_level, constraints) => {
                        if other_id == &id {
                            // TODO: Should be RecursiveRowType?
                            return Err(Error::RecursiveType);
                        } else if other_level > level {
                            *other = TypeVar::UnboundRow(level, constraints);
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

    fn inject_constraints(
        &mut self,
        is_variant: bool,
        constraints: Constraints,
        ty: &Type,
    ) -> Result<()> {
        match ty {
            Type::Var(id) => {
                let type_var = self.get_mut_type_var(*id)?;
                match type_var.clone() {
                    TypeVar::Link(ty) => self.inject_constraints(false, constraints, &ty),
                    TypeVar::UnboundRow(level, other_constraints) => {
                        /* TODO: Why did I have this before?
                        if !is_variant {
                            let mut intersection = constraints.intersection(&other_constraints);
                            if let Some(label) = intersection.next() {
                                println!(
                                    "unbound row: {:?}, {}",
                                    intersection,
                                    self.ty_to_string(ty).unwrap()
                                );
                                return Err(Error::RowConstraintFailed(label.clone()));
                            }
                        }
                        */
                        let constraints = constraints.union(&other_constraints).cloned().collect();
                        *type_var = TypeVar::UnboundRow(level, constraints);
                        Ok(())
                    }
                    TypeVar::GenericRow(other_constraints) => {
                        if !is_variant {
                            let mut intersection = constraints.intersection(&other_constraints);
                            if let Some(label) = intersection.next() {
                                println!("generic row: {:?}", intersection);
                                return Err(Error::RowConstraintFailed(label.clone()));
                            }
                        }
                        let constraints = constraints.union(&other_constraints).cloned().collect();
                        *type_var = TypeVar::GenericRow(constraints);
                        Ok(())
                    }
                    _ => {
                        let ty = self.ty_to_string(ty)?;
                        Err(Error::CannotInjectConstraintsInto(ty))
                    }
                }
            }
            Type::Record(ty) => self.inject_constraints(false, constraints, ty),
            Type::Variant(ty) => self.inject_constraints(true, constraints, ty),
            Type::RowExtend(_, _) => {
                let (labels, rest) = self.match_row_ty(ty)?;
                for label in labels.keys() {
                    if !is_variant && constraints.contains(label) {
                        println!("row extend");
                        return Err(Error::RowConstraintFailed(label.clone()));
                    }
                }
                self.inject_constraints(is_variant, constraints, &rest)?;
                Ok(())
            }
            Type::RowEmpty => Ok(()),
            _ => {
                let ty = self.ty_to_string(ty)?;
                Err(Error::CannotInjectConstraintsInto(ty))
            }
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        self.unify_inner(false, ty1, ty2)
    }

    fn unify_inner(&mut self, is_variant: bool, ty1: &Type, ty2: &Type) -> Result<()> {
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
                    self.unify_inner(is_variant, arg1, arg2)?;
                }
                self.unify_inner(is_variant, app_ty1, app_ty2)
            }
            (Type::Arrow(params1, ret1), Type::Arrow(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return self.cannot_unify(ty1, ty2);
                }
                for i in 0..params1.len() {
                    let param1 = &params1[i];
                    let param2 = &params2[i];
                    self.unify_inner(is_variant, param1, param2)?;
                }
                self.unify_inner(is_variant, ret1, ret2)
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
                    TypeVar::UnboundRow(level, constraints) => {
                        self.inject_constraints(is_variant, constraints, ty2)?;
                        self.occurs_check_adjust_levels(*id, level, ty2)?;
                        self.link(*id, ty2.clone())
                    }
                    TypeVar::Link(ty1) => self.unify_inner(is_variant, &ty1, ty2),
                    TypeVar::Generic => self.cannot_unify(ty1, ty2),
                    TypeVar::GenericRow(_) => self.cannot_unify(ty1, ty2),
                }
            }
            (_, Type::Var(id)) => {
                let tvar = self.get_type_var(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty1)?;
                        self.link(*id, ty1.clone())
                    }
                    TypeVar::UnboundRow(level, constraints) => {
                        self.inject_constraints(is_variant, constraints, ty1)?;
                        self.occurs_check_adjust_levels(*id, level, ty1)?;
                        self.link(*id, ty1.clone())
                    }
                    TypeVar::Link(ty2) => self.unify_inner(is_variant, ty1, &ty2),
                    TypeVar::Generic => self.cannot_unify(ty1, ty2),
                    TypeVar::GenericRow(_) => self.cannot_unify(ty1, ty2),
                }
            }
            (Type::Record(row1), Type::Record(row2)) => self.unify_inner(false, row1, row2),
            (Type::Variant(row1), Type::Variant(row2)) => self.unify_inner(true, row1, row2),
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
                            let rest = self.new_unbound_row(level, Constraints::new());
                            self.unify(&rest2, &Type::RowExtend(missing2, rest.clone().into()))?;
                            if let TypeVar::Link(_) = self.get_type_var(id)? {
                                return Err(Error::RecursiveRowType);
                            }
                            self.unify(&rest1, &Type::RowExtend(missing1, rest.into()))
                        }
                        TypeVar::UnboundRow(level, constraints) => {
                            let rest = self.new_unbound_row(level, constraints);
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
                    TypeVar::UnboundRow(other_level, constraints) if other_level > level => {
                        *type_var = TypeVar::GenericRow(constraints);
                        Ok(())
                    }
                    TypeVar::Link(ty) => self.generalize(level, &ty),
                    TypeVar::Unbound(_)
                    | TypeVar::UnboundRow(_, _)
                    | TypeVar::Generic
                    | TypeVar::GenericRow(_) => Ok(()),
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
                    TypeVar::Unbound(_) | TypeVar::UnboundRow(_, _) => Ok(ty),
                    TypeVar::Link(ty) => self.instantiate_inner(id_vars, level, ty),
                    TypeVar::Generic => {
                        let ty = id_vars.entry(id).or_insert_with(|| self.new_unbound(level));
                        Ok(ty.clone())
                    }
                    TypeVar::GenericRow(constraints) => {
                        let ty = id_vars
                            .entry(id)
                            .or_insert_with(|| self.new_unbound_row(level, constraints));
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
                    TypeVar::UnboundRow(_, _) | TypeVar::Generic | TypeVar::GenericRow(_) => {
                        Err(Error::ExpectedFunction)
                    }
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

    fn assign_pattern(&mut self, pattern: &Pattern, ty: Type) -> Result<()> {
        match (pattern, ty) {
            (Pattern::Var(name), ty) => self.insert_var(name.clone(), ty),
            (Pattern::Record(labels), Type::Record(row)) => {
                let (labels_ty, _) = self.match_row_ty(&row)?;
                for (label, label_pattern) in labels {
                    match labels_ty.get(label) {
                        None => return Err(Error::MissingLabel(label.clone())),
                        Some(label_ty) => self.assign_pattern(label_pattern, label_ty.clone())?,
                    }
                }
            }
            (Pattern::Record(_), ty) => {
                let ty = self.ty_to_string(&ty)?;
                return Err(Error::RecordPatternNotRecord(ty));
            }
        }
        Ok(())
    }

    fn infer_pattern(&mut self, level: Level, pattern: &Pattern) -> Type {
        match pattern {
            Pattern::Var(name) => {
                let ty = self.new_unbound(level);
                self.insert_var(name.clone(), ty.clone());
                ty
            }
            Pattern::Record(labels) => {
                let constraints = labels.keys().cloned().collect();
                let labels = labels
                    .iter()
                    .map(|(label, pat)| (label.clone(), self.infer_pattern(level, pat)))
                    .collect();
                let rest = self.new_unbound_row(level, constraints);
                Type::Record(Type::RowExtend(labels, rest.into()).into())
            }
        }
    }

    fn infer_inner(&mut self, level: Level, expr: &Expr) -> Result<Type> {
        match expr {
            Expr::Bool(_) => Ok(Type::bool()),
            Expr::Int(_) => Ok(Type::int()),
            Expr::IntBinOp(op, lhs, rhs) => {
                let ty = Type::int();
                let lhs_ty = self.infer_inner(level, lhs)?;
                self.unify(&ty, &lhs_ty)?;
                let rhs_ty = self.infer_inner(level, rhs)?;
                self.unify(&ty, &rhs_ty)?;
                Ok(op.output_ty())
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
                Ok(Type::bool())
            }
            Expr::Var(name) => {
                let ty = self.get_var(name)?.clone();
                self.instantiate(level, ty)
            }
            Expr::Fun(params, body) => {
                let mut param_tys = Vec::with_capacity(params.len());
                let old_vars = self.vars.clone();
                for param in params {
                    let param_ty = self.infer_pattern(level, param);
                    param_tys.push(param_ty);
                }
                let ret_ty = self.infer_inner(level, body)?;
                self.vars = old_vars;
                Ok(Type::Arrow(param_tys, ret_ty.into()))
            }
            Expr::Let(pattern, value, body) => {
                let var_ty = self.infer_inner(level + 1, value)?;
                self.generalize(level, &var_ty)?;
                self.assign_pattern(pattern, var_ty)?;
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
                let rest = self.new_unbound_row(level, Constraints::singleton(label.clone()));
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
                let rest = self.new_unbound_row(level, Constraints::singleton(label.clone()));
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
                let constraints = labels.keys().cloned().collect();
                for (label, expr) in labels {
                    let ty = self.infer_inner(level, expr)?;
                    tys.insert(label.clone(), ty);
                }
                let rest = self.new_unbound_row(level, constraints);
                let record = self.infer_inner(level, record)?;
                self.unify(&Type::Record(rest.clone().into()), &record)?;
                Ok(Type::Record(Type::RowExtend(tys, rest.into()).into()))
            }
            Expr::Variant(label, expr) => {
                let rest = self.new_unbound_row(level, Constraints::singleton(label.clone()));
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
                let constraints = cases.iter().map(|(label, _, _)| label).cloned().collect();
                let def_variant = self.new_unbound_row(level, constraints);
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
            Expr::If(if_expr, if_body, elifs, else_body) => {
                let bool = Type::bool();
                let ty = self.infer_inner(level, if_expr)?;
                self.unify(&bool, &ty)?;
                let if_ty = self.infer_inner(level, if_body)?;
                for (elif_expr, elif_body) in elifs {
                    let ty = self.infer_inner(level, elif_expr)?;
                    self.unify(&bool, &ty)?;
                    let ty = self.infer_inner(level, elif_body)?;
                    self.unify(&if_ty, &ty)?;
                }
                let ty = self.infer_inner(level, else_body)?;
                self.unify(&if_ty, &ty)?;
                // TODO: if calling a function with an open variant should keep it open
                match if_ty {
                    Type::Variant(row) => {
                        let (labels, _) = self.match_row_ty(&row)?;
                        Ok(Type::Variant(
                            Type::RowExtend(labels, Type::RowEmpty.into()).into(),
                        ))
                    }
                    _ => Ok(if_ty),
                }
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

    pub fn replace_ty_constants_with_vars(&mut self, forall: ForAll, mut ty: Type) -> Type {
        if !forall.vars.is_empty() || !forall.row_vars.is_empty() {
            let mut env = HashMap::new();
            for var in forall.vars {
                let ty = self.new_generic();
                env.insert(var, ty);
            }
            for (row_var, constraints) in forall.row_vars {
                let ty = self.new_generic_row(constraints);
                env.insert(row_var, ty);
            }
            ty = ty.replace_const_with_vars(&env);
        }
        ty
    }

    pub fn ty_to_string(&self, ty: &Type) -> Result<String> {
        let mut namer = Namer::new();
        let mut ty = self.ty_to_string_impl(&mut namer, ty, false)?;
        if !namer.names.is_empty() || !namer.row_names.is_empty() {
            let names: BTreeSet<String> = namer
                .names
                .into_values()
                .map(|name| (name as char).to_string())
                .collect();
            let row_names: BTreeMap<String, Constraints> = namer
                .row_names
                .into_iter()
                .map(|(id, name)| {
                    let type_var = self.get_type_var(id)?;
                    match type_var {
                        TypeVar::GenericRow(constraints) => {
                            let name = format!("r{}", name as char);
                            Ok((name, constraints.clone()))
                        }
                        _ => unreachable!(),
                    }
                })
                .collect::<Result<BTreeMap<_, _>, Error>>()?;
            let args = names.iter().chain(row_names.keys()).join(" ");
            let mut constraints = row_names
                .iter()
                .filter(|(_, constraints)| !constraints.is_empty())
                .map(|(row_name, constraints)| {
                    format!("{}\\{}", row_name, constraints.iter().join("\\"))
                })
                .join(", ");
            if !constraints.is_empty() {
                constraints = format!(". ({})", constraints);
            }
            ty = format!("forall {}{} => {}", args, constraints, ty);
        }
        Ok(ty)
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

    fn ty_to_string_impl(&self, namer: &mut Namer, ty: &Type, is_simple: bool) -> Result<String> {
        match ty {
            Type::Const(name) => Ok(name.clone()),
            Type::App(ty, args) => {
                let mut ty_str = self.ty_to_string_impl(namer, ty, true)?;
                ty_str.push('[');
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        ty_str.push_str(", ")
                    }
                    let arg = self.ty_to_string_impl(namer, arg, false)?;
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
                    let param = self.ty_to_string_impl(namer, &params[0], true)?;
                    let ret = self.ty_to_string_impl(namer, ret, false)?;
                    ty_str.push_str(&param);
                    ty_str.push_str(" -> ");
                    ty_str.push_str(&ret);
                } else {
                    ty_str.push('(');
                    for (i, param) in params.iter().enumerate() {
                        if i != 0 {
                            ty_str.push_str(", ");
                        }
                        let param = self.ty_to_string_impl(namer, param, false)?;
                        ty_str.push_str(&param);
                    }
                    let ret = self.ty_to_string_impl(namer, ret, false)?;
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
                        let name = namer.get_or_insert_name(*id);
                        Ok(name.to_string())
                    }
                    TypeVar::GenericRow(_) => {
                        let name = namer.get_or_insert_row_name(*id);
                        Ok(format!("r{}", name))
                    }
                    TypeVar::Unbound(_) => Ok(format!("_{}", id)),
                    TypeVar::UnboundRow(_, _) => Ok(format!("_row{}", id)),
                    TypeVar::Link(ty) => self.ty_to_string_impl(namer, ty, is_simple),
                }
            }
            Type::Record(row) => {
                let ty = self.ty_to_string_impl(namer, row, is_simple)?;
                Ok(format!("{{{}}}", ty))
            }
            Type::Variant(row) => {
                let ty = self.ty_to_string_impl(namer, row, is_simple)?;
                Ok(format!("[{}]", ty))
            }
            Type::RowEmpty => Ok("".to_string()),
            Type::RowExtend(_, _) => {
                let (labels, rest) = self.match_row_ty(ty)?;
                let mut output = labels
                    .into_iter()
                    .map(|(label, ty)| {
                        self.ty_to_string_impl(namer, &ty, is_simple)
                            .map(|ty| format!("{}: {}", label, ty))
                    })
                    .collect::<Result<Vec<String>>>()?
                    .iter()
                    .join(", ");
                match self.real_ty(rest)? {
                    Type::RowEmpty => (),
                    Type::RowExtend(_, _) => unreachable!(),
                    other_ty => {
                        output.push_str(" | ");
                        let other_ty = self.ty_to_string_impl(namer, &other_ty, is_simple)?;
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

    #[track_caller]
    fn pass(expr_str: &str, expected: &str) {
        let (forall, ty) = Parser::ty(expected).unwrap();
        let mut env = make_env();
        let expected = env.replace_ty_constants_with_vars(forall, ty);
        let expr = Parser::expr(expr_str).unwrap();
        let actual = env.infer_inner(0, &expr).unwrap();
        env.generalize(-1, &actual).unwrap();
        let expected = env.ty_to_string(&expected).unwrap();
        let actual = env.ty_to_string(&actual).unwrap();
        assert_eq!(expected, actual, "for {}", expr_str);
    }

    #[track_caller]
    fn fail(expr_str: &str, expected: Error) {
        let mut env = make_env();
        let expr = Parser::expr(expr_str).unwrap();
        let actual = env.infer_inner(0, &expr).unwrap_err();
        assert_eq!(expected, actual, "for {}", expr_str);
    }

    #[test]
    fn infer_pattern() {
        pass("let x = 1 in x", "int");
        pass("let {x = x} = {x = 1} in x", "int");
        pass("let {x} = {x = 1} in x", "int");
        pass("let {x = {y = y}} = {x = {y = 1}} in y", "int");
    }

    #[test]
    fn infer_base() {
        pass("id", "forall a => a -> a");
        pass("one", "int");
        fail("x", Error::VariableNotFound("x".to_owned()));
        fail("let x = x in x", Error::VariableNotFound("x".to_owned()));
        pass("let x = id in x", "forall a => a -> a");
        pass("let x = fun(y) -> y in x", "forall a => a -> a");
        pass("fun(x) -> x", "forall a => a -> a");
        pass("pair", "forall a b => (a, b) -> pair[a, b]");
        pass("pair", "forall z x => (x, z) -> pair[x, z]");
        pass(
            "fun(x) -> let y = fun(z) -> z in y",
            "forall a b => a -> b -> b",
        );
        pass(
            "let f = fun(x) -> x in let id = fun(y) -> y in eq(f, id)",
            "bool",
        );
        pass(
            "let f = fun(x) -> x in let id = fun(y) -> y in eq_curry(f)(id)",
            "bool",
        );
        pass("let f = fun(x) -> x in eq(f, succ)", "bool");
        pass("let f = fun(x) -> x in eq_curry(f)(succ)", "bool");
        pass(
            "let f = fun(x) -> x in pair(f(one), f(true))",
            "pair[int, bool]",
        );
        fail(
            "fun(f) -> pair(f(one), f(true))",
            Error::CannotUnify("bool".to_owned(), "int".to_owned()),
        );
        pass(
            "let f = fun(x, y) -> let a = eq(x, y) in eq(x, y) in f",
            "forall a => (a, a) -> bool",
        );
        pass(
            "let f = fun(x, y) -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f",
            "forall a => (a, a) -> bool",
        );
        pass("id(id)", "forall a => a -> a");
        pass(
            "choose(fun(x, y) -> x, fun(x, y) -> y)",
            "forall a => (a, a) -> a",
        );
        pass(
            "choose_curry(fun(x, y) -> x)(fun(x, y) -> y)",
            "forall a => (a, a) -> a",
        );
        pass(
            "let x = id in let y = let z = x(id) in z in y",
            "forall a => a -> a",
        );
        pass("cons(id, nil)", "forall a => list[a -> a]");
        pass("cons_curry(id)(nil)", "forall a => list[a -> a]");
        pass(
            "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2",
            "list[int -> int]",
        );
        pass(
            "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))",
            "list[int -> int]",
        );
        fail(
            "plus(one, true)",
            Error::CannotUnify("bool".to_owned(), "int".to_owned()),
        );
        fail("plus(one)", Error::UnexpectedNumberOfArguments);
        pass("fun(x) -> let y = x in y", "forall a => a -> a");
        pass(
            "fun(x) -> let y = let z = x(fun(x) -> x) in z in y",
            "forall a b => ((a -> a) -> b) -> b",
        );
        pass(
            "fun(x) -> fun(y) -> let x = x(y) in x(y)",
            "forall a b => (a -> a -> b) -> a -> b",
        );
        pass(
            "fun(x) -> let y = fun(z) -> x(z) in y",
            "forall a b => (a -> b) -> a -> b",
        );
        pass(
            "fun(x) -> let y = fun(z) -> x in y",
            "forall a b => a -> b -> a",
        );
        pass(
            "fun(x) -> fun(y) -> let x = x(y) in fun(x) -> y(x)",
            "forall a b c => ((a -> b) -> c) -> (a -> b) -> a -> b",
        );
        fail("fun(x) -> let y = x in y(y)", Error::RecursiveType);
        pass(
            "fun(x) -> let y = fun(z) -> z in y(y)",
            "forall a b => a -> b -> b",
        );
        fail("fun(x) -> x(x)", Error::RecursiveType);
        fail("one(id)", Error::ExpectedFunction);
        pass(
            "fun(f) -> let x = fun(g, y) -> let _ = g(y) in eq(f, g) in x",
            "forall a b => (a -> b) -> (a -> b, a) -> bool",
        );
        pass(
            "let const = fun(x) -> fun(y) -> x in const",
            "forall a b => a -> b -> a",
        );
        pass(
            "let apply = fun(f, x) -> f(x) in apply",
            "forall a b => (a -> b, a) -> b",
        );
        pass(
            "let apply_curry = fun(f) -> fun(x) -> f(x) in apply_curry",
            "forall a b => (a -> b) -> a -> b",
        );
        pass("1 == 1", "bool");
        pass("1 > 2", "bool");
    }

    #[test]
    fn infer_records() {
        pass("{}", "{}");
        fail("{}.x", Error::MissingLabel("x".to_owned()));
        pass("{a = one}", "{a : int}");
        pass("{a = one, b = true}", "{a : int, b : bool}");
        pass("{b = true, a = one}", "{b : bool, a : int}");
        pass("{a = one, b = true}.a", "int");
        pass("{a = one, b = true}.b", "bool");
        fail("{a = one, b = true}.c", Error::MissingLabel("c".to_owned()));
        pass("{f = fun(x) -> x}", "forall a => {f : a -> a}");
        pass(
            "let r = {a = id, b = succ} in choose(r.a, r.b)",
            "int -> int",
        );
        pass(
            "let r = {a = id, b = fun(x) -> x} in choose(r.a, r.b)",
            "forall a => a -> a",
        );
        fail("choose({a = one}, {})", Error::MissingLabel("a".to_owned()));
        pass("{ x = zero | { y = one | {} } }", "{y : int, x : int}");
        pass(
            "choose({ x = zero | { y = one | {} } }, {x = one, y = zero})",
            "{y : int, x : int}",
        );
        fail("{}\\x", Error::MissingLabel("x".to_owned()));
        pass("{x = one, y = zero} \\ x", "{y : int}");
        pass("{ x = true | {x = one}\\x}", "{x : bool}");
        pass("let a = {} in {b = one | a}", "{b : int}");
        pass("let a = {x = one} in {x = true | a\\x}.x", "bool");
        fail(
            "let a = {x = one} in a.y",
            Error::MissingLabel("y".to_owned()),
        );
        pass("let a = {x = one} in a \\ x", "{}");
        fail(
            "let a = {x = one} in let b = {x = true | a\\x} in b\\x.x",
            Error::MissingLabel("x".to_owned()),
        );
        pass(
            "fun(r) -> {x = one | r}",
            "forall ra. (ra\\x) => {ra} -> {x : int | ra}",
        );
        pass("fun(r) -> r.x", "forall ra a. (ra\\x) => {x : a | ra} -> a");
        pass(
            "let get_x = fun(r) -> r.x in get_x({y = one, x = zero})",
            "int",
        );
        fail(
            "let get_x = fun(r) -> r.x in get_x({y = one, z = true})",
            Error::MissingLabel("x".to_owned()),
        );
        pass(
            "fun(r) -> choose({x = zero | r}, {x = one | {}})",
            "{} -> {x : int}",
        );
        pass(
            "fun(r) -> choose({x = zero | r}, {x = one})",
            "{} -> {x : int}",
        );
        pass(
            "fun(r) -> choose({x = zero | r}, {x = one | r})",
            "forall ra. (ra\\x) => {ra} -> {x : int | ra}",
        );
        fail(
            "fun(r) -> choose({x = zero | r}, {y = one | r})",
            Error::RowConstraintFailed("y".to_owned()),
        );
        pass("let f = fun(x) -> x.t(one) in f({t = succ})", "int");
        pass("let f = fun(x) -> x.t(one) in f({t = id})", "int");
        fail(
            "let f = fun(r) -> let y = r.y in choose(r, {x = one}) in f",
            Error::MissingLabel("y".to_owned()),
        );
        fail(
            "fun(r) -> choose({x = zero | r}, {x = true | r})",
            Error::CannotUnify("int".to_owned(), "bool".to_owned()),
        );
        pass(
            "fun(r, s) -> choose({b = true, c = zero | r}, {b = false, c = one, d = half | s})",
            "forall ra. (ra\\b\\c\\d) => ({d : float | ra}, {ra}) -> {b : bool, c : int, d : float | ra}",
        );
        pass(
            "fun(r) -> {x = r | r}",
            "forall ra. (ra\\x) => {ra} -> {x : {ra} | ra}",
        );
        pass("let { x = x } = { x = 1 } in { x = x }", "{ x: int}");
    }

    #[test]
    fn infer_variant() {
        pass(":X one", "forall ra. (ra\\X) => [X : int | ra]");
        pass(
            "choose(choose(:x one, :Y true), choose(:X half, :y nil))",
            "forall a ra. (ra\\X\\Y\\x\\y) => [X : float, Y : bool, x : int, y : list[a] | ra]",
        );
        fail(
            "choose(:X one, :X true)",
            Error::CannotUnify("int".to_owned(), "bool".to_owned()),
        );
        pass(
            "choose(:X {x = one, y = false}, :Y {w = half})",
            "forall ra. (ra\\X\\Y) => [X : {x : int, y : bool}, Y : {w : float} | ra]",
        );
        fail(
            concat!(
                "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
                "match e { :x i -> i , :Y y -> zero}"
            ),
            Error::MissingLabel("X".to_owned()),
        );
        pass(
            "fun(x, y) -> match x {:a i -> one , :b i -> zero , :c i -> y}",
            "forall a b c => ([a : a, b : b, c : c], int) -> int",
        );
        pass(
            "fun(a) -> match a {:X i -> i , r -> one}",
            "forall ra. (ra\\X) => [X : int | ra] -> int",
        );
        pass(
            concat!(
                "let f = fun(m) -> match m {:y a -> one , :Y b -> zero , :z z -> zero} in ",
                "fun(e) -> match e { :x i -> i , :X f -> one , r -> f(r)}"
            ),
            "forall a b c d => [X : a, Y : b, x : int, y : c, z : d] -> int",
        );
        pass(
            concat!(
                "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
                "let f = fun(m) -> match m {:y a -> one , :Y b -> zero , :z z -> zero} in ",
                "match e { :x i -> i , :X f -> one , r -> f(r)}"
            ),
            "int",
        );
        pass(
            "fun(e) -> match e { :X a -> plus(a.x, one) }",
            "forall ra. (ra\\x) => [X : {x : int | ra}] -> int",
        );
    }
}
