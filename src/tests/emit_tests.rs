use crate::{
    emit::{self, LOAD_NAME},
    infer,
    parser::Parser,
};
use wasmi::*;

#[track_caller]
fn pass(source: &str, expected: i64) {
    let mut env = infer::Env::default();
    let expr = Parser::expr(source).unwrap();
    let typed_expr = env.infer(expr.clone()).unwrap();
    let wat = emit::emit(typed_expr).unwrap();
    let wasm = wat::parse_str(wat).unwrap();
    let engine = Engine::default();
    let module = Module::new(&engine, &mut &wasm[..]).unwrap();
    let mut store = Store::new(&engine, ());
    let linker: Linker<()> = Linker::new(&engine);
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let load = instance
        .get_typed_func::<(), i64>(&store, LOAD_NAME)
        .unwrap();
    let actual = load.call(&mut store, ()).unwrap();
    assert_eq!(expected, actual);
}

#[test]
fn test() {
    pass("let add(a, b) = a + b in add(1, 2)", 3);
}
