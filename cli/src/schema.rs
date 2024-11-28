pub mod backends {
    use std::io;

    pub trait Backend {
        type Input<'a>;
        type Value;
        type Error;
        fn parse<'a>(s: Self::Input<'a>) -> Result<Self::Value, Self::Error>;

        fn write(v: &Self::Value, w: impl io::Write) -> io::Result<()>;
    }

    #[cfg(feature = "json")]
    pub mod json_backend {
        use std::io;

        pub struct JsonBackend;

        impl super::Backend for JsonBackend {
            type Input<'a> = &'a str;

            type Value = json::JsonValue;

            type Error = json::Error;

            fn parse<'a>(s: &'a str) -> Result<json::JsonValue, json::Error> {
                json::parse(s)
            }

            fn write(v: &json::JsonValue, mut w: impl io::Write) -> io::Result<()> {
                v.write(&mut w)
            }
        }
    }
}

pub mod values {
    use super::backends::Backend;

    pub trait SchemaValue<B: Backend>: Sized {
        type SerErr;
        type DeserErr;
        fn serialize(self) -> Result<<B as Backend>::Value, Self::SerErr>;
        fn deserialize(s: <B as Backend>::Value) -> Result<Self, Self::DeserErr>;
    }

    #[cfg(feature = "json")]
    pub mod json_value {
        use super::*;
        use crate::schema::backends::json_backend::JsonBackend;

        impl SchemaValue<JsonBackend> for bool {
            type SerErr = ();
            type DeserErr = String;

            fn serialize(self) -> Result<json::JsonValue, ()> {
                Ok(json::JsonValue::Boolean(self))
            }
            fn deserialize(s: json::JsonValue) -> Result<Self, String> {
                match s {
                    json::JsonValue::Boolean(value) => Ok(value),
                    s => Err(format!("non-boolean value {s}")),
                }
            }
        }

        impl SchemaValue<JsonBackend> for String {
            type SerErr = ();
            type DeserErr = String;

            fn serialize(self) -> Result<json::JsonValue, ()> {
                Ok(json::JsonValue::String(self))
            }
            fn deserialize(s: json::JsonValue) -> Result<Self, String> {
                match s {
                    json::JsonValue::String(value) => Ok(value),
                    s => Err(format!("non-string value {s}")),
                }
            }
        }
    }

    /* pub enum SchemaValue { */
    /*     Bool(bool), */
    /*     Path(PathBuf), */
    /* } */

    /* pub trait SchemaValue<B: Backend> {} */

    /* impl SchemaValue for bool {} */
}

pub mod transformers {
    use super::backends::Backend;

    use std::{error, ffi, fmt, io, marker::PhantomData, str};

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

        fn write(v: &<B as Backend>::Value, w: impl io::Write) -> io::Result<()> {
            <B as Backend>::write(v, w)
        }
    }
}

#[cfg(test)]
mod test {
    use super::{backends::Backend, *};
    use std::{ffi, io};

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
        fn write(v: &bool, mut w: impl io::Write) -> io::Result<()> {
            match v {
                true => w.write_all(b"true"),
                false => w.write_all(b"false"),
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
        use std::str;
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
