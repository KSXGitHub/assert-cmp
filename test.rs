#![no_implicit_prelude]

// TODO: remove this line once https://github.com/rust-lang/rust/issues/84357 is resolved.
use ::std::panic;

#[test]
fn assert_op_passes() {
    ::assert_cmp::assert_op!(123 < 456);
    ::assert_cmp::assert_op!(456 > 123);
    ::assert_cmp::assert_op!("abc" == "abc");
    let left = 123;
    let right = 456;
    ::assert_cmp::assert_op!(left < right);
    ::assert_cmp::assert_op!(left < 456);
    ::assert_cmp::assert_op!(123 < right);
}

#[test]
#[should_panic(expected = "123 > 456 ⇒ 123 > 456 ⇒ false")]
fn assert_op_literal_literal_fails() {
    ::assert_cmp::assert_op!(123 > 456);
}

#[test]
#[should_panic(expected = "123 > right ⇒ 123 > 456 ⇒ false")]
fn assert_op_literal_ident_fails() {
    let right = 456;
    ::assert_cmp::assert_op!(123 > right);
}

#[test]
#[should_panic(expected = "left > 456 ⇒ 123 > 456 ⇒ false")]
fn assert_op_ident_literal_fails() {
    let left = 123;
    ::assert_cmp::assert_op!(left > 456);
}

#[test]
#[should_panic(expected = "left > right ⇒ 123 > 456 ⇒ false")]
fn assert_op_ident_ident_fails() {
    let left = 123;
    let right = 456;
    ::assert_cmp::assert_op!(left > right);
}

#[test]
fn assert_op_expr_passes() {
    ::assert_cmp::assert_op_expr!(12 + 34, ==, 34 + 12);
    ::assert_cmp::assert_op_expr!(12 + 34, !=, 43 + 21);
}

#[test]
#[should_panic(expected = "12 + 34 == 43 + 21 ⇒ 46 == 64 ⇒ false")]
fn assert_op_expr_fails() {
    ::assert_cmp::assert_op_expr!(12 + 34, ==, 43 + 21);
}

#[test]
fn assert_fn_passes() {
    let eq = |a, b| a == b;
    ::assert_cmp::assert_fn!(eq(12 + 34, 34 + 12));
}

#[test]
#[should_panic(expected = "eq(12 + 34, 43 + 21) ⇒ eq(46, 64) ⇒ false")]
fn assert_fn_fails() {
    let eq = |a, b| a == b;
    ::assert_cmp::assert_fn!(eq(12 + 34, 43 + 21));
}
