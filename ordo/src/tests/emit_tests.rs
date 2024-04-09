use crate::{
    emit::{self, LOAD_NAME},
    infer,
    parser::Parser,
};
use wasmi::*;

#[track_caller]
fn pass<T: WasmResults + PartialEq + std::fmt::Debug>(source: &str, expected: T) {
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
    let load = instance.get_typed_func::<(), T>(&store, LOAD_NAME).unwrap();
    let actual = load.call(&mut store, ()).unwrap();
    assert_eq!(expected, actual);
}

#[test]
fn test() {
    pass("let add(a, b) = a + b in add(1, 2)", 3_u64);
    pass("let a = 1 in a", 1_u64);
    pass("4 * 5 / 2 - 1 + 4", 13_u64);
    pass("2 == 2", 1_i32);
    pass("3 > 4", 0_i32);
}
