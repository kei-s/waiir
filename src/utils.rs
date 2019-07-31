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

And with custom result.

```
#[derive(Debug,PartialEq)]
pub enum Object {
    Integer(i64),
    Null => "null"
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(x) => write!(f, "{}", x),
            Object::Null(x) => write!(f, "null"),
        }
    }
}
```
*/
#[macro_export]
macro_rules! enum_with_fmt {
    ($name:ident; $($var:ident($ty:ty)),+; $($null:ident => $expr:expr),*) => {
        #[derive(Debug,PartialEq)]
        pub enum $name {
            $(
                $var($ty),
            )+
            $(
                $null,
            )*
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $name::$var(x) => write!(f, "{}", x),
                    )+
                    $(
                        $name::$null => write!(f, $expr),
                    )*
                }
            }
        }
    };
}
