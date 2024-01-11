use core::fmt;
use std::collections::{btree_map::Entry, BTreeMap, BTreeSet, HashMap};

use itertools::Itertools;

use crate::{
    expr::{
        Constraints, Expr, ExprAt, ExprIn, ExprTypedAt, Id, Level, Pattern, PatternAt,
        PatternTypedAt, Type, TypeVar, OK_LABEL,
    },
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
    ExpectedFunction(String),
    VariableNotFound(String),
    CannotInjectConstraintsInto(String),
    RowConstraintFailed(String),
    RecordPatternNotRecord(String),
    UnwrapMissingOk(String),
    UnwrapNotVariant(String),
    PatternRecordRestNotEmpty(ExprAt),
    InvalidPattern(ExprAt),
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
            Error::ExpectedFunction(ty) => write!(f, "expected a function, got: {}", ty),
            Error::VariableNotFound(var) => write!(f, "variable not found: {}", var),
            Error::CannotInjectConstraintsInto(ty) => {
                write!(f, "cannot inject constraints into: {}", ty)
            }
            Error::RowConstraintFailed(label) => {
                write!(f, "row constraint failed for label: {}", label)
            }
            Error::RecordPatternNotRecord(ty) => write!(f, "record pattern not a record: {}", ty),
            Error::UnwrapMissingOk(ty) => write!(f, "unwrap missing ok case, type: {}", ty),
            Error::UnwrapNotVariant(ty) => {
                write!(f, "unwrap on something other than variant: {}", ty)
            }
            Error::PatternRecordRestNotEmpty(expr) => {
                write!(f, "record pattern extended something: {}", expr)
            }
            Error::InvalidPattern(expr) => write!(f, "invalid pattern: {}", expr),
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
    wrap: BTreeMap<String, Type>,
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
                        let ty = self.ty_to_string(&ty)?;
                        Err(Error::ExpectedFunction(ty))
                    }
                }
            }
            _ => {
                let ty = self.ty_to_string(&ty)?;
                Err(Error::ExpectedFunction(ty))
            }
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

    pub fn infer(&mut self, expr: ExprAt) -> Result<ExprTypedAt> {
        let expr = self.infer_inner(0, expr)?;
        let expr = self.wrapped(expr)?;
        self.generalize(-1, expr.ty())?;
        Ok(expr)
    }

    fn assign_pattern(&mut self, pattern: PatternAt, ty: Type) -> Result<PatternTypedAt> {
        match (*pattern.expr, ty) {
            (Pattern::Var(name), ty) => {
                self.insert_var(name.clone(), ty.clone());
                Ok(Pattern::Var(name).with(pattern.context, ty))
            }
            (Pattern::RecordExtend(labels, rest), Type::Record(row)) => {
                match *rest.expr {
                    Pattern::RecordEmpty => (),
                    _ => return Err(Error::PatternRecordRestNotEmpty(rest.clone())),
                }
                let (labels_ty, rest_ty) = self.match_row_ty(&row)?;
                let mut label_patterns = BTreeMap::new();
                for (label, label_pattern) in labels {
                    match labels_ty.get(&label) {
                        None => return Err(Error::MissingLabel(label.clone())),
                        Some(label_ty) => {
                            let label_pattern =
                                self.assign_pattern(label_pattern, label_ty.clone())?;
                            label_patterns.insert(label, label_pattern);
                        }
                    }
                }
                let rest = Pattern::RecordEmpty.with(rest.context, rest_ty);
                Ok(Pattern::RecordExtend(label_patterns, rest)
                    .with(pattern.context, Type::Record(row)))
            }
            (Pattern::RecordExtend(_, _), ty) => {
                let ty = self.ty_to_string(&ty)?;
                Err(Error::RecordPatternNotRecord(ty))
            }
            (expr, _) => Err(Error::InvalidPattern(ExprIn {
                context: pattern.context,
                expr: expr.into(),
            })),
        }
    }

    fn infer_pattern(&mut self, level: Level, pattern: PatternAt) -> Result<PatternTypedAt> {
        match *pattern.expr {
            Pattern::Var(name) => {
                let ty = self.new_unbound(level);
                self.insert_var(name.clone(), ty.clone());
                Ok(Pattern::Var(name).with(pattern.context, ty))
            }
            Pattern::RecordExtend(labels, rest) => {
                match *rest.expr {
                    Pattern::RecordEmpty => (),
                    _ => return Err(Error::PatternRecordRestNotEmpty(rest.clone())),
                }
                let constraints = labels.keys().cloned().collect();
                let mut label_exprs = BTreeMap::new();
                let mut label_tys = BTreeMap::new();
                for (label, pat) in labels {
                    let label_expr = self.infer_pattern(level, pat)?;
                    label_tys.insert(label.clone(), label_expr.ty().clone());
                    label_exprs.insert(label, label_expr);
                }
                let rest_ty = self.new_unbound_row(level, constraints);
                let ty = Type::Record(Type::RowExtend(label_tys, rest_ty.clone().into()).into());
                let rest = Pattern::RecordEmpty.with(rest.context, rest_ty);
                Ok(Pattern::RecordExtend(label_exprs, rest).with(pattern.context, ty))
            }
            _ => Err(Error::InvalidPattern(pattern.clone())),
        }
    }

    fn wrap_with(&mut self, labels: BTreeMap<String, Type>) -> Result<()> {
        for (label, ty) in labels {
            match self.wrap.entry(label) {
                Entry::Vacant(v) => {
                    v.insert(ty);
                }
                Entry::Occupied(o) => {
                    let old_ty = o.get().clone();
                    self.unify(&old_ty, &ty)?;
                }
            }
        }
        Ok(())
    }

    fn wrapped(&mut self, mut expr: ExprTypedAt) -> Result<ExprTypedAt> {
        let mut labels = std::mem::take(&mut self.wrap);
        if labels.is_empty() {
            return Ok(expr);
        }
        labels.insert(OK_LABEL.to_owned(), expr.ty().clone());
        let rest = Type::RowEmpty;
        let ty = Type::Variant(Type::RowExtend(labels, rest.into()).into());
        expr.context.ty.ty = ty;
        Ok(expr)
    }

    fn infer_inner(&mut self, level: Level, expr: ExprAt) -> Result<ExprTypedAt> {
        match *expr.expr {
            Expr::Bool(b) => Ok(Expr::Bool(b).with(expr.context, Type::bool())),
            Expr::Int(i) => Ok(Expr::Int(i).with(expr.context, Type::int())),
            Expr::IntBinOp(op, lhs, rhs) => {
                let ty = Type::int();
                let lhs = self.infer_inner(level, lhs)?;
                self.unify(&ty, lhs.ty())?;
                let rhs = self.infer_inner(level, rhs)?;
                self.unify(&ty, rhs.ty())?;
                let ty = op.output_ty();
                Ok(Expr::IntBinOp(op, lhs, rhs).with(expr.context, ty))
            }
            Expr::Negate(value) => {
                let ty = Type::bool();
                let value = self.infer_inner(level, value)?;
                self.unify(&ty, value.ty())?;
                Ok(Expr::Negate(value).with(expr.context, ty))
            }
            Expr::EqualEqual(lhs, rhs) => {
                let lhs = self.infer_inner(level, lhs)?;
                let rhs = self.infer_inner(level, rhs)?;
                self.unify(lhs.ty(), rhs.ty())?;
                Ok(Expr::EqualEqual(lhs, rhs).with(expr.context, Type::bool()))
            }
            Expr::Var(name) => {
                let ty = self.get_var(&name)?.clone();
                let ty = self.instantiate(level, ty)?;
                Ok(Expr::Var(name).with(expr.context, ty))
            }
            Expr::Fun(params, body) => {
                let mut param_exprs = Vec::with_capacity(params.len());
                let mut param_tys = Vec::with_capacity(params.len());
                let old_vars = self.vars.clone();
                let old_wrap = self.wrap.clone();
                for param in params {
                    let param_expr = self.infer_pattern(level, param)?;
                    param_tys.push(param_expr.ty().clone());
                    param_exprs.push(param_expr);
                }
                let body = self.infer_inner(level, body)?;
                self.vars = old_vars;
                self.wrap = old_wrap;
                let ty = Type::Arrow(param_tys, body.ty().clone().into());
                Ok(Expr::Fun(param_exprs, body).with(expr.context, ty))
            }
            Expr::Let(pattern, value, body) => {
                let value = self.infer_inner(level + 1, value)?;
                self.generalize(level, value.ty())?;
                let pattern = self.assign_pattern(pattern, value.ty().clone())?;
                let body = self.infer_inner(level, body)?;
                let ty = body.ty().clone();
                Ok(Expr::Let(pattern, value, body).with(expr.context, ty))
            }
            Expr::Call(fun, args) => {
                let fun = self.infer_inner(level, fun)?;
                let (params, ret) = self.match_fun_ty(args.len(), fun.ty().clone())?;
                let mut typed_args = Vec::with_capacity(args.len());
                for (i, arg) in args.into_iter().enumerate() {
                    let arg = self.infer_inner(level, arg)?;
                    let param = &params[i];
                    self.unify(arg.ty(), param)?;
                    typed_args.push(arg);
                }
                Ok(Expr::Call(fun, typed_args).with(expr.context, *ret))
            }
            Expr::RecordEmpty => {
                let ty = Type::Record(Type::RowEmpty.into());
                Ok(Expr::RecordEmpty.with(expr.context, ty))
            }
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
                self.unify(&param, record.ty())?;
                Ok(Expr::RecordSelect(record, label).with(expr.context, ret))
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
                self.unify(&param, record.ty())?;
                Ok(Expr::RecordRestrict(record, label).with(expr.context, ret))
            }
            Expr::RecordExtend(labels, record) => {
                let mut tys = BTreeMap::new();
                let constraints = labels.keys().cloned().collect();
                let mut typed_labels = BTreeMap::new();
                for (label, expr) in labels {
                    let expr = self.infer_inner(level, expr)?;
                    tys.insert(label.clone(), expr.ty().clone());
                    typed_labels.insert(label, expr);
                }
                let rest = self.new_unbound_row(level, constraints);
                let record = self.infer_inner(level, record)?;
                self.unify(&Type::Record(rest.clone().into()), record.ty())?;
                let ty = Type::Record(Type::RowExtend(tys, rest.into()).into());
                Ok(Expr::RecordExtend(typed_labels, record).with(expr.context, ty))
            }
            Expr::Variant(label, value) => {
                let rest = self.new_unbound_row(level, Constraints::singleton(label.clone()));
                let variant = self.new_unbound(level);
                let param = variant.clone();
                let ret = Type::Variant(
                    Type::RowExtend(BTreeMap::singleton(label.clone(), variant), rest.into())
                        .into(),
                );
                let value = self.infer_inner(level, value)?;
                self.unify(&param, value.ty())?;
                Ok(Expr::Variant(label, value).with(expr.context, ret))
            }
            Expr::Case(value, cases, None) => {
                let ret = self.new_unbound(level);
                let value = self.infer_inner(level, value)?;
                let (cases_row, cases) = self.infer_cases(level, &ret, Type::RowEmpty, cases)?;
                self.unify(value.ty(), &Type::Variant(cases_row.into()))?;
                Ok(Expr::Case(value, cases, None).with(expr.context, ret))
            }
            Expr::Case(value, cases, Some((def_var, def_expr))) => {
                let constraints = cases.iter().map(|(label, _, _)| label).cloned().collect();
                let def_variant = self.new_unbound_row(level, constraints);
                let old_vars = self.vars.clone();
                self.vars
                    .insert(def_var.clone(), Type::Variant(def_variant.clone().into()));
                let def_expr = self.infer_inner(level, def_expr)?;
                let ret = def_expr.ty().clone();
                self.vars = old_vars;
                let value = self.infer_inner(level, value)?;
                let (cases_row, cases) = self.infer_cases(level, &ret, def_variant, cases)?;
                self.unify(value.ty(), &Type::Variant(cases_row.into()))?;
                Ok(Expr::Case(value, cases, Some((def_var, def_expr))).with(expr.context, ret))
            }
            Expr::Unwrap(value) => {
                let value = self.infer_inner(level, value)?;
                match value.ty() {
                    Type::Variant(rows) => {
                        let (mut labels, _) = self.match_row_ty(rows)?;
                        match labels.remove(OK_LABEL) {
                            None => {
                                let ty = self.ty_to_string(value.ty())?;
                                Err(Error::UnwrapMissingOk(ty))
                            }
                            Some(ty) => {
                                self.wrap_with(labels)?;
                                Ok(Expr::Unwrap(value).with(expr.context, ty))
                            }
                        }
                    }
                    _ => {
                        let ty = self.ty_to_string(value.ty())?;
                        Err(Error::UnwrapNotVariant(ty))
                    }
                }
            }
            Expr::If(if_expr, if_body, elifs, else_body) => {
                let bool = Type::bool();
                let if_expr = self.infer_inner(level, if_expr)?;
                self.unify(&bool, if_expr.ty())?;
                let if_body = self.infer_inner(level, if_body)?;
                let mut typed_elifs = Vec::with_capacity(elifs.len());
                for (elif_expr, elif_body) in elifs {
                    let elif_expr = self.infer_inner(level, elif_expr)?;
                    self.unify(&bool, elif_expr.ty())?;
                    let elif_body = self.infer_inner(level, elif_body)?;
                    self.unify(if_body.ty(), elif_body.ty())?;
                    typed_elifs.push((elif_expr, elif_body));
                }
                let else_body = self.infer_inner(level, else_body)?;
                self.unify(if_body.ty(), else_body.ty())?;
                // TODO: if calling a function with an open variant should keep it open
                match if_body.ty().clone() {
                    Type::Variant(row) => {
                        let (labels, _rest) = self.match_row_ty(&row)?;
                        let ty =
                            Type::Variant(Type::RowExtend(labels, Type::RowEmpty.into()).into());
                        Ok(Expr::If(if_expr, if_body, typed_elifs, else_body)
                            .with(expr.context, ty))
                    }
                    ty => {
                        Ok(Expr::If(if_expr, if_body, typed_elifs, else_body)
                            .with(expr.context, ty))
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn infer_cases(
        &mut self,
        level: Level,
        ret: &Type,
        rest: Type,
        cases: Vec<(String, String, ExprAt)>,
    ) -> Result<(Type, Vec<(String, String, ExprTypedAt)>)> {
        let mut labels = BTreeMap::new();
        let mut typed_cases = Vec::with_capacity(cases.len());
        for (label, var, case) in cases {
            let variant = self.new_unbound(level);
            let old_vars = self.vars.clone();
            self.vars.insert(var.clone(), variant.clone());
            let case = self.infer_inner(level, case)?;
            self.vars = old_vars;
            self.unify(ret, case.ty())?;
            labels.insert(label.clone(), variant);
            typed_cases.push((label, var, case));
        }
        let ty = Type::RowExtend(labels, rest.into());
        Ok((ty, typed_cases))
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
