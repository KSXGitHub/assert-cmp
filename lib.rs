//! Convenient assertion macros that print the failed expressions and their evaluated values.
#![no_std]
#![no_implicit_prelude]

/// Create assertion macros for boolean binary operators.
#[macro_export]
macro_rules! operator_assertion_macros {
    (
        $(#![$shared_attr:meta])*
        in $(::$module:ident)+;
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
                $(::$module)+::$name_expr!($left, $op, $right)
            };
            ($left:ident $op:tt $right:literal) => {
                $(::$module)+::$name_expr!($left, $op, $right)
            };
            ($left:literal $op:tt $right:ident) => {
                $(::$module)+::$name_expr!($left, $op, $right)
            };
            ($left:literal $op:tt $right:literal) => {
                $(::$module)+::$name_expr!($left, $op, $right)
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
                            "{func}({left_expr}, {right_expr}) ⇒ {func}({left_value:?}, {right_value:?}) ⇒ false",
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

    /// Assert that a binary expression of 2 identifiers/literals returns `true`.
    ///
    /// **Syntax:**
    ///
    /// ```ignore
    /// assert_op!($left $op $right)
    /// ```
    ///
    /// * `$left` and `$right` are either identifiers or literals or both.
    /// * `$op` is a binary operator (e.g. `>`, `<`, `>=`, `<=`, `==`, `!=`).
    ///
    /// **Example:** An assertion that passes
    ///
    /// ```
    /// # use assert_cmp::assert_op;
    /// assert_op!(123 < 456);
    /// ```
    ///
    /// **Example:** An assertion that fails
    ///
    /// ```should_panic
    /// # use assert_cmp::assert_op;
    /// assert_op!(123 > 456); // panic: 123 > 456 ⇒ 123 > 456 ⇒ false
    /// ```
    simple = assert_op;

    /// Assert that a binary expression of 2 expressions returns `true`.
    ///
    /// **Syntax:**
    ///
    /// ```ignore
    /// assert_op_expr!($left, $op, $right)
    /// ```
    ///
    /// * `$left` and `$right` are expressions.
    /// * `$op` is a binary operator (e.g. `>`, `<`, `>=`, `<=`, `==`, `!=`).
    ///
    /// **Example:** An assertion that passes
    ///
    /// ```
    /// # use assert_cmp::assert_op_expr;
    /// assert_op_expr!(12 + 34, ==, 34 + 12);
    /// ```
    ///
    /// **Example:** An assertion that fails
    ///
    /// ```should_panic
    /// # use assert_cmp::assert_op_expr;
    /// assert_op_expr!(12 + 34, ==, 43 + 21); // panic: 12 + 34 == 43 + 21 ⇒ 46 == 64 ⇒ false
    /// ```
    expr = assert_op_expr;
}

operator_assertion_macros! {
    #![macro_export]
    in ::assert_cmp;
    use ::core::debug_assert;

    /// Assert that a binary expression of 2 identifiers/literals returns `true`.
    ///
    /// This macro is the debug-only version of [`assert_op`].
    /// It acts like `assert_op` in debug mode, but does nothing in release mode.
    simple = debug_assert_op;

    /// Assert that a binary expression of 2 expressions returns `true`.
    ///
    /// This macro is the debug-only version of [`assert_op`].
    /// It acts like `assert_op_expr` in debug mode, but does nothing in release mode.
    expr = debug_assert_op_expr;
}

function_assertion_macro! {
    #![macro_export]
    use ::core::assert;

    /// Assert that a binary function call of 2 expressions returns `true`.
    ///
    /// **Syntax:**
    ///
    /// ```ignore
    /// assert_fn!($function($left, $right))
    /// ```
    ///
    /// ```ignore
    /// assert_fn!(not $function($left, $right))
    /// ```
    ///
    /// * `$function` is an identifier of a binary function.
    /// * `$left` and `$right` are expressions.
    /// * `not`'s appearance means expecting the function call to returns `false` instead of `true`.
    ///
    /// **Example:** An assertion that passes
    ///
    /// ```
    /// # use assert_cmp::assert_fn;
    /// fn func<A, B>(_: A, _: B) -> bool {
    ///   true
    /// }
    /// assert_fn!(func(123, 456));
    /// ```
    ///
    /// **Example:** An assertion that fails
    ///
    /// ```should_panic
    /// # use assert_cmp::assert_fn;
    /// fn func<A, B>(_: A, _: B) -> bool {
    ///   false
    /// }
    /// assert_fn!(func(123, 456)); // panic: func(123, 456) ⇒ func(123, 456) ⇒ false
    /// ```
    ///
    /// **Example:** A negative assertion that passes
    ///
    /// ```
    /// # use assert_cmp::assert_fn;
    /// fn func<A, B>(_: A, _: B) -> bool {
    ///   false
    /// }
    /// assert_fn!(not func(123, 456));
    /// ```
    ///
    /// **Example:** A negative assertion that fails
    ///
    /// ```should_panic
    /// # use assert_cmp::assert_fn;
    /// fn func<A, B>(_: A, _: B) -> bool {
    ///   true
    /// }
    /// assert_fn!(not func(123, 456)); // panic: func(123, 456) ⇒ func(123, 456) ⇒ true
    /// ```
    assert_fn;
}

function_assertion_macro! {
    #![macro_export]
    use ::core::debug_assert;

    /// Assert that a binary function call of 2 expressions returns `true`.
    ///
    /// This macro is the debug-only version of [`assert_fn`].
    /// It acts like `assert_op_expr` in debug mode, but does nothing in release mode.
    debug_assert_fn;
}
