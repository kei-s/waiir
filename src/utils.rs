/*
Enum with fmt generation macro

This macro generates below code.

```
#[derive(Debug,PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ...
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement(x) => write!(f, "{}", x),
            Statement::ReturnStatement(x) => write!(f, "{}", x),
            ...
        }
    }
}
```

Additionally, this macro generate custom output.

```
pub enum E {
    V1(T),
    =>
    V2(T) => "Custom: {}",
    ;
    V3,
    =>
    V4 => "Custom",
}
```
*/
#[macro_export]
macro_rules! enum_with_fmt {
    (
        $(#[$meta: meta])*
        pub enum $name: ident {
            $($a_var: ident ($a_ty: ty),)*
        }
    ) => {
        enum_with_fmt!(
            $(#[$meta])*
            pub enum $name {
                $($a_var ($a_ty),)*
                =>
                ;
                =>
            }
        );
    };
    (
        $(#[$meta: meta])*
        pub enum $name: ident {
            $($a_var: ident ($a_ty: ty),)*
            =>
            $($b_var: ident ($b_ty: ty) => $b_expr: expr,)*
            ;
            $($c_var: ident)*
            =>
            $($d_var: ident => $d_expr: expr,)*
        }
    ) => {
        $(#[$meta])*
        pub enum $name {
            $($a_var($a_ty),)*
            $($b_var($b_ty),)*
            $($c_var,)*
            $($d_var,)*
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $($name::$a_var(x) => write!(f, "{}", x),)*
                    $($name::$b_var(x) => write!(f, $b_expr, x),)*
                    $($name::$c_var => write!(f, "{}", x),)*
                    $($name::$d_var => write!(f, $d_expr),)*
                }
            }
        }
    };
}
