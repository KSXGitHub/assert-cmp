//! Convenient assertion macros that print the failed expressions and their evaluated values.
#![no_std]
#![no_implicit_prelude]

/// Create assertion macros for boolean binary operators.
#[macro_export]
macro_rules! operator_assertion_macros {
    (
        $(#![$shared_attr:meta])*
        in $module:path;
        use $assert:path;
        $(#[$attr_simple:meta])* simple = $name_simple:ident;
        $(#[$attr_expr:meta])* expr = $name_expr:ident;
    ) => {
        $(#[$shared_attr])*
        $(#[$attr_expr])*
        macro_rules! $name_expr {
            ($left:expr, $op:tt, $right:expr) => {
                match ($left, $right) {
                    (left, right) => {
                        $assert!(
                            left $op right,
                            "{left_expr} {op} {right_expr} ⇒ {left_value:?} {op} {right_value:?} ⇒ false",
                            op = stringify!($op),
                            left_expr = stringify!($left),
                            right_expr = stringify!($right),
                            left_value = left,
                            right_value = right,
                        )
                    }
                }
            };
        }

        $(#[$shared_attr])*
        $(#[$attr_simple])*
        macro_rules! $name_simple {
            ($left:ident $op:tt $right:ident) => {
                $module::$name_expr!($left, $op, $right)
            };
            ($left:ident $op:tt $right:literal) => {
                $module::$name_expr!($left, $op, $right)
            };
            ($left:literal $op:tt $right:ident) => {
                $module::$name_expr!($left, $op, $right)
            };
            ($left:literal $op:tt $right:literal) => {
                $module::$name_expr!($left, $op, $right)
            };
        }
    };
}

/// Create an assertion macro for boolean function calls.
#[macro_export]
macro_rules! function_assertion_macro {
    (
        $(#![$shared_attr:meta])*
        use $assert:path;
        $(#[$attr:meta])* $name:ident;
    ) => {
        $(#[$shared_attr])*
        $(#[$attr])*
        macro_rules! $name {
            ($function:ident($left:expr, $right:expr)) => {
                match ($left, $right) {
                    (left, right) => {
                        $assert!(
                            $function($left, $right),
                            "{func}({left_expr}, {right_expr}) ⇒ {func}({left_value}, {right_value}) ⇒ false",
                            func = stringify!($function),
                            left_expr = stringify!($left),
                            right_expr = stringify!($right),
                            left_value = left,
                            right_value = right,
                        )
                    }
                }
            };

            (not $function:ident($left:expr, $right:expr)) => {
                match ($left, $right) {
                    (left, right) => {
                        $assert!(
                            !$function($left, $right),
                            "{func}({left_expr}, {right_expr}) ⇒ {func}({left_value}, {right_value}) ⇒ true",
                            func = stringify!($function),
                            left_expr = stringify!($left),
                            right_expr = stringify!($right),
                            left_value = left,
                            right_value = right,
                        )
                    }
                }
            };
        }
    };
}

operator_assertion_macros! {
    #![macro_export]
    in ::assert_cmp;
    use ::core::assert;

    #[doc = "Assert that a binary expression of 2 identifiers/literals returns `true`."]
    #[doc = ""]
    #[doc = "**Syntax:**"]
    #[doc = "```ignore"]
    #[doc = "assert_op!($left $op $right)"]
    #[doc = "```"]
    #[doc = "* `$left` and `$right` are either identifiers or literals or both."]
    #[doc = "* `$op` is a binary operator (e.g. `>`, `<`, `>=`, `<=`, `==`, `!=`)."]
    #[doc = ""]
    #[doc = "**Example:** An assertion that passes"]
    #[doc = "```"]
    #[doc = "# use assert_cmp::assert_op;"]
    #[doc = "assert_op!(123 < 456);"]
    #[doc = "```"]
    #[doc = ""]
    #[doc = "**Example:** An assertion that fails"]
    #[doc = "```should_panic"]
    #[doc = "# use assert_cmp::assert_op;"]
    #[doc = "assert_op!(123 > 456); // panic: 123 > 456 ⇒ 123 > 456 ⇒ false"]
    #[doc = "```"]
    simple = assert_op;

    #[doc = "Assert that a binary expression of 2 expressions returns `true`."]
    #[doc = ""]
    #[doc = "**Syntax:**"]
    #[doc = "```ignore"]
    #[doc = "assert_op_expr!($left, $op, $right)"]
    #[doc = "```"]
    #[doc = "* `$left` and `$right` are expressions."]
    #[doc = "* `$op` is a binary operator (e.g. `>`, `<`, `>=`, `<=`, `==`, `!=`)."]
    #[doc = ""]
    #[doc = "**Example:** An assertion that passes"]
    #[doc = "```"]
    #[doc = "# use assert_cmp::assert_op_expr;"]
    #[doc = "assert_op_expr!(12 + 34, ==, 34 + 12);"]
    #[doc = "```"]
    #[doc = ""]
    #[doc = "**Example:** An assertion that fails"]
    #[doc = "```should_panic"]
    #[doc = "# use assert_cmp::assert_op_expr;"]
    #[doc = "assert_op_expr!(12 + 34, ==, 43 + 21); // panic: 12 + 34 == 43 + 21 ⇒ 46 == 64 ⇒ false"]
    #[doc = "```"]
    expr = assert_op_expr;
}

operator_assertion_macros! {
    #![macro_export]
    in ::assert_cmp;
    use ::core::debug_assert;

    #[doc = "Assert that a binary expression of 2 identifiers/literals returns `true`."]
    #[doc = ""]
    #[doc = "This macro is the debug-only version of [`assert_op`]."]
    #[doc = "It acts like `assert_op` in debug mode, but does nothing in release mode."]
    simple = debug_assert_op;

    #[doc = "Assert that a binary expression of 2 expressions returns `true`."]
    #[doc = ""]
    #[doc = "This macro is the debug-only version of [`assert_op`]."]
    #[doc = "It acts like `assert_op_expr` in debug mode, but does nothing in release mode."]
    expr = debug_assert_op_expr;
}

function_assertion_macro! {
    #![macro_export]
    use ::core::assert;

    #[doc = "Assert that a binary function call of 2 expressions returns `true`."]
    #[doc = ""]
    #[doc = "**Syntax:**"]
    #[doc = "```ignore"]
    #[doc = "assert_fn!($function($left, $right))"]
    #[doc = "```"]
    #[doc = "```ignore"]
    #[doc = "assert_fn!(not $function($left, $right))"]
    #[doc = "```"]
    #[doc = "* `$function` is an identifier of a binary function."]
    #[doc = "* `$left` and `$right` are expressions."]
    #[doc = "* `not`'s appearance means expecting the function call to returns `false` instead of `true`."]
    #[doc = ""]
    #[doc = "**Example:** An assertion that passes"]
    #[doc = "```"]
    #[doc = "# use assert_cmp::assert_fn;"]
    #[doc = "fn func<A, B>(_: A, _: B) -> bool {"]
    #[doc = "  true"]
    #[doc = "}"]
    #[doc = "assert_fn!(func(123, 456));"]
    #[doc = "```"]
    #[doc = ""]
    #[doc = "**Example:** An assertion that fails"]
    #[doc = "```should_panic"]
    #[doc = "# use assert_cmp::assert_fn;"]
    #[doc = "fn func<A, B>(_: A, _: B) -> bool {"]
    #[doc = "  false"]
    #[doc = "}"]
    #[doc = "assert_fn!(func(123, 456)); // panic: func(123, 456) ⇒ func(123, 456) ⇒ false"]
    #[doc = "```"]
    #[doc = ""]
    #[doc = "**Example:** A negative assertion that passes"]
    #[doc = "```"]
    #[doc = "# use assert_cmp::assert_fn;"]
    #[doc = "fn func<A, B>(_: A, _: B) -> bool {"]
    #[doc = "  false"]
    #[doc = "}"]
    #[doc = "assert_fn!(not func(123, 456));"]
    #[doc = "```"]
    #[doc = ""]
    #[doc = "**Example:** A negative assertion that fails"]
    #[doc = "```should_panic"]
    #[doc = "# use assert_cmp::assert_fn;"]
    #[doc = "fn func<A, B>(_: A, _: B) -> bool {"]
    #[doc = "  true"]
    #[doc = "}"]
    #[doc = "assert_fn!(not func(123, 456)); // panic: func(123, 456) ⇒ func(123, 456) ⇒ true"]
    #[doc = "```"]
    assert_fn;
}

function_assertion_macro! {
    #![macro_export]
    use ::core::debug_assert;

    #[doc = "Assert that a binary function call of 2 expressions returns `true`."]
    #[doc = ""]
    #[doc = "This macro is the debug-only version of [`assert_fn`]."]
    #[doc = "It acts like `assert_op_expr` in debug mode, but does nothing in release mode."]
    debug_assert_fn;
}
