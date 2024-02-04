use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ordo::{infer::Env, parser::Parser};

pub fn infer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("infer");

    let expr = "let a = 1 in a";
    group.bench_function(expr, |b| {
        b.iter(|| {
            let source = black_box(expr);
            let parsed = Parser::expr(source).unwrap();
            let mut env = black_box(Env::default());
            let typed = env.infer(parsed).unwrap();
            black_box(typed)
        })
    });

    let expr = "2 + 3 * 4";
    group.bench_function(expr, |b| {
        b.iter(|| {
            let source = black_box(expr);
            let parsed = Parser::expr(source).unwrap();
            let mut env = black_box(Env::default());
            let typed = env.infer(parsed).unwrap();
            black_box(typed)
        })
    });

    let expr = "let add(a, b) = a + b in add(2, 3)";
    group.bench_function(expr, |b| {
        b.iter(|| {
            let source = black_box(expr);
            let parsed = Parser::expr(source).unwrap();
            let mut env = black_box(Env::default());
            let typed = env.infer(parsed).unwrap();
            black_box(typed)
        })
    });

    let expr = "let add({a, b}) = a + b in let a = 2 in let b = 3 in add({a, b})";
    group.bench_function(expr, |b| {
        b.iter(|| {
            let source = black_box(expr);
            let parsed = Parser::expr(source).unwrap();
            let mut env = black_box(Env::default());
            let typed = env.infer(parsed).unwrap();
            black_box(typed)
        })
    });

    let expr =
        "let safe_div(n, d) = if d == 0 then :div_by_zero {} else :ok (n / d) in safe_div(4, 2)";
    group.bench_function(expr, |b| {
        b.iter(|| {
            let source = black_box(expr);
            let parsed = Parser::expr(source).unwrap();
            let mut env = black_box(Env::default());
            let typed = env.infer(parsed).unwrap();
            black_box(typed)
        })
    });
}

criterion_group!(benches, infer_benchmark);
criterion_main!(benches);
