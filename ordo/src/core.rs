use crate::{infer::Env, parser::Parser};

#[track_caller]
fn assume(env: &mut Env, name: &str, sig: &str) {
    let (forall, ty) = Parser::ty(sig).unwrap();
    let ty = env.replace_ty_constants_with_vars(forall, ty);
    env.insert_var(name.to_owned(), ty);
}

pub fn make_env() -> Env {
    let mut env = Env::default();
    assume(&mut env, "head", "forall a => list[a] -> a");
    assume(&mut env, "tail", "forall a => list[a] -> list[a]");
    assume(&mut env, "nil", "forall a => list[a]");
    assume(&mut env, "cons", "forall a => (a, list[a]) -> list[a]");
    assume(
        &mut env,
        "cons_curry",
        "forall a => a -> list[a] -> list[a]",
    );
    assume(
        &mut env,
        "map",
        "forall a b => (a -> b, list[a]) -> list[b]",
    );
    assume(
        &mut env,
        "map_curry",
        "forall a b => (a -> b) -> list[a] -> list[b]",
    );
    assume(&mut env, "one", "int");
    assume(&mut env, "zero", "int");
    assume(&mut env, "half", "float");
    assume(&mut env, "succ", "int -> int");
    assume(&mut env, "plus", "(int, int) -> int");
    assume(&mut env, "eq", "forall a => (a, a) -> bool");
    assume(&mut env, "eq_curry", "forall a => a -> a -> bool");
    assume(&mut env, "not", "bool -> bool");
    assume(&mut env, "pair", "forall a b => (a, b) -> pair[a, b]");
    assume(&mut env, "pair_curry", "forall a b => a -> b -> pair[a, b]");
    assume(&mut env, "first", "forall a b => pair[a, b] -> a");
    assume(&mut env, "second", "forall a b => pair[a, b] -> b");
    assume(&mut env, "id", "forall a => a -> a");
    assume(&mut env, "const", "forall a b => a -> b -> a");
    assume(&mut env, "apply", "forall a b => (a -> b, a) -> b");
    assume(&mut env, "apply_curry", "forall a b => (a -> b) -> a -> b");
    assume(&mut env, "choose", "forall a => (a, a) -> a");
    assume(&mut env, "choose_curry", "forall a => a -> a -> a");
    env
}
