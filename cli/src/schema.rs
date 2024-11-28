use std::str;

pub trait Schema {}

/* pub enum SchemaValue { */
/*     Bool(bool), */
/*     Path(PathBuf), */
/* } */

/* pub trait SchemaValue<B: Backend> {} */

/* impl SchemaValue for bool {} */

pub trait Backend {
    type Input<'a>;
    type Value;
    type Error;
    fn parse<'a>(s: Self::Input<'a>) -> Result<Self::Value, Self::Error>;
}

pub mod transformers {
    use super::Backend;

    use std::{error, ffi, fmt, marker::PhantomData, str};

    pub trait Transformer {
        type A<'a>;
        type B<'a>;
        type Error;
        fn convert_input<'a>(s: Self::A<'a>) -> Result<Self::B<'a>, Self::Error>;
    }

    pub struct StrTransformer;

    impl Transformer for StrTransformer {
        type A<'a> = &'a ffi::OsStr;
        type B<'a> = &'a str;
        type Error = str::Utf8Error;
        fn convert_input<'a>(s: Self::A<'a>) -> Result<Self::B<'a>, Self::Error> {
            s.try_into()
        }
    }

    pub struct DecoderTransformer<T, B>(PhantomData<(T, B)>);

    impl<T, B> DecoderTransformer<T, B> {
        pub const fn new() -> Self {
            Self(PhantomData)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum WrapperError<In, Out> {
        In(In),
        Out(Out),
    }

    impl<In, Out> fmt::Display for WrapperError<In, Out>
    where
        In: fmt::Display,
        Out: fmt::Display,
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::In(e) => e.fmt(f),
                Self::Out(e) => e.fmt(f),
            }
        }
    }

    impl<In, Out> error::Error for WrapperError<In, Out>
    where
        In: error::Error,
        Out: error::Error,
    {
        fn source(&self) -> Option<&(dyn error::Error + 'static)> {
            match self {
                Self::In(e) => e.source(),
                Self::Out(e) => e.source(),
            }
        }
    }

    impl<T, B> Backend for DecoderTransformer<T, B>
    where
        T: Transformer,
        for<'a> B: Backend<Input<'a> = <T as Transformer>::B<'a>>,
    {
        type Input<'a> = <T as Transformer>::A<'a>;
        type Value = <B as Backend>::Value;
        type Error = WrapperError<<T as Transformer>::Error, <B as Backend>::Error>;
        fn parse<'a>(s: Self::Input<'a>) -> Result<Self::Value, Self::Error> {
            let s: <T as Transformer>::B<'a> =
                <T as Transformer>::convert_input(s).map_err(|e| WrapperError::In(e))?;
            <B as Backend>::parse(s).map_err(|e| WrapperError::Out(e))
        }
    }
}

pub struct JsonBackend;

#[cfg(feature = "json")]
impl Backend for JsonBackend {
    type Input<'a> = &'a str;
    type Value = json::JsonValue;
    type Error = json::Error;
    fn parse<'a>(s: &'a str) -> Result<Self::Value, Self::Error> {
        json::parse(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::ffi;

    struct BoolBackend;
    impl Backend for BoolBackend {
        type Input<'a> = &'a str;
        type Value = bool;
        type Error = String;
        fn parse<'a>(s: &'a str) -> Result<bool, String> {
            match s {
                "true" => Ok(true),
                "false" => Ok(false),
                e => Err(e.to_string()),
            }
        }
    }

    #[test]
    fn parse_bool() {
        assert!(BoolBackend::parse("true").unwrap());
        assert!(!BoolBackend::parse("false").unwrap());
        assert_eq!(BoolBackend::parse("").err().unwrap(), "");
        assert_eq!(BoolBackend::parse("aaaaasdf").err().unwrap(), "aaaaasdf");
    }

    #[cfg(unix)]
    mod unix {
        use std::{ffi, os::unix::ffi::OsStrExt};

        pub fn broken_utf8() -> &'static ffi::OsStr {
            // Here, the values 0x66 and 0x6f correspond to 'f' and 'o'
            // respectively. The value 0x80 is a lone continuation byte, invalid
            // in a UTF-8 sequence.
            ffi::OsStr::from_bytes(&[0x66, 0x6f, 0x80, 0x6f])
        }
    }
    #[cfg(windows)]
    mod windows {
        use std::{ffi, os::windows::ffi::OsStringExt};

        pub fn broken_utf8() -> ffi::OsString {
            // Here the values 0x0066 and 0x006f correspond to 'f' and 'o'
            // respectively. The value 0xD800 is a lone surrogate half, invalid
            // in a UTF-16 sequence.
            ffi::OsString::from_wide(&[0x0066, 0x006f, 0xD800, 0x006f])
        }
    }
    fn broken_utf8() -> std::ffi::OsString {
        #[cfg(unix)]
        let broken = unix::broken_utf8().to_os_string();
        #[cfg(windows)]
        let broken = windows::broken_utf8();

        broken
    }

    #[test]
    fn utf8_parse_failure() {
        let broken = broken_utf8();
        assert!(broken.to_str().is_none());
    }

    #[test]
    fn str_wrapper() {
        use transformers::{DecoderTransformer, StrTransformer, WrapperError};
        type Wrapper = DecoderTransformer<StrTransformer, BoolBackend>;

        assert!(Wrapper::parse(ffi::OsStr::new("true")).unwrap());
        assert!(!Wrapper::parse(ffi::OsStr::new("false")).unwrap());
        assert_eq!(
            Wrapper::parse(ffi::OsStr::new("")).err().unwrap(),
            WrapperError::Out(String::from(""))
        );
        assert_eq!(
            Wrapper::parse(ffi::OsStr::new("aaaaasdf")).err().unwrap(),
            WrapperError::Out(String::from("aaaaasdf"))
        );

        let broken = broken_utf8();
        assert_eq!(
            Wrapper::parse(broken.as_ref()).err().unwrap(),
            WrapperError::In(str::from_utf8(broken.as_encoded_bytes()).err().unwrap()),
        );
    }
}
