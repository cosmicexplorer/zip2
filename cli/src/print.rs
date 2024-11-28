pub mod printer {
    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct PrintOptions {
        pub wrap_width: usize,
    }

    impl Default for PrintOptions {
        fn default() -> Self {
            Self { wrap_width: 80 }
        }
    }

    #[derive(Debug, Clone)]
    struct PrintContext {
        prefix: String,
        value_column: Option<usize>,
    }

    impl PrintContext {
        pub const fn new() -> Self {
            Self {
                prefix: String::new(),
                value_column: None,
            }
        }
    }

    pub struct Printer {
        opts: PrintOptions,
        ctx: PrintContext,
    }

    impl Printer {
        pub const fn create(opts: PrintOptions) -> Self {
            Self {
                opts,
                ctx: PrintContext::new(),
            }
        }
    }
}

pub trait HelpSection {}

pub enum HelpVerbosity {
    NameOnly,
    NameAndDescription,
    CompleteWithCaveats,
}
