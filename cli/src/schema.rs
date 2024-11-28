use std::{error, ffi, fmt, marker::PhantomData, str};

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

pub struct StrDecoderTransformer<B>(PhantomData<B>);

impl<B> StrDecoderTransformer<B> {
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StrWrapperError<E> {
    Utf8(str::Utf8Error),
    Unwrap(E),
}

impl<E> fmt::Display for StrWrapperError<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Utf8(e) => <str::Utf8Error as fmt::Display>::fmt(e, f),
            Self::Unwrap(e) => <E as fmt::Display>::fmt(e, f),
        }
    }
}

impl<E> error::Error for StrWrapperError<E>
where
    E: error::Error,
{
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Utf8(e) => e.source(),
            Self::Unwrap(e) => e.source(),
        }
    }
}

impl<B> Backend for StrDecoderTransformer<B>
where
    for<'a> B: Backend<Input<'a> = &'a str>,
{
    type Input<'a> = &'a ffi::OsStr;
    type Value = <B as Backend>::Value;
    type Error = StrWrapperError<<B as Backend>::Error>;
    fn parse<'a>(s: &'a ffi::OsStr) -> Result<Self::Value, Self::Error> {
        let s: &'a str = s
            .try_into()
            .map_err(|e: str::Utf8Error| StrWrapperError::Utf8(e))?;
        <B as Backend>::parse(s).map_err(|e| StrWrapperError::Unwrap(e))
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
    fn broken_utf8() -> ffi::OsString {
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
        type Wrapper = StrDecoderTransformer<BoolBackend>;

        assert!(Wrapper::parse(ffi::OsStr::new("true")).unwrap());
        assert!(!Wrapper::parse(ffi::OsStr::new("false")).unwrap());
        assert_eq!(
            Wrapper::parse(ffi::OsStr::new("")).err().unwrap(),
            StrWrapperError::Unwrap(String::from(""))
        );
        assert_eq!(
            Wrapper::parse(ffi::OsStr::new("aaaaasdf")).err().unwrap(),
            StrWrapperError::Unwrap(String::from("aaaaasdf"))
        );

        let broken = broken_utf8();
        assert_eq!(
            Wrapper::parse(broken.as_ref()).err().unwrap(),
            StrWrapperError::Utf8(str::from_utf8(broken.as_encoded_bytes()).err().unwrap()),
        );
    }
}
