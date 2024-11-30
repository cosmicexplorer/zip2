use super::OutputType;
use crate::args::resource::*;

use std::{collections::VecDeque, ffi::OsString, path::PathBuf};

pub enum OutputTypeError {
    ArgWith(&'static str, String),
    ArgTwice(&'static str),
    NoValFor(&'static str),
    ValArgTwice {
        arg: &'static str,
        prev: String,
        new: String,
    },
}

impl Resource for OutputType {
    const ID: &'static str = "OUTPUT-FLAGS";
}

impl ArgvResource for OutputType {
    type ArgvParseError = OutputTypeError;
    fn parse_argv(argv: &mut VecDeque<OsString>) -> Result<Self, Self::ArgvParseError> {
        let mut allow_stdout: bool = false;
        let mut append_to_output_path: bool = false;
        let mut output_path: Option<PathBuf> = None;

        while let Some(arg) = argv.pop_front() {
            match arg.as_encoded_bytes() {
                b"--stdout" => {
                    if let Some(output_path) = output_path.take() {
                        return Err(OutputTypeError::ArgWith(
                            "--stdout",
                            format!("output file {output_path:?}"),
                        ));
                    }
                    if append_to_output_path {
                        return Err(OutputTypeError::ArgWith("--stdout", "--append".to_string()));
                    }
                    if allow_stdout {
                        return Err(OutputTypeError::ArgTwice("--stdout"));
                    }
                    allow_stdout = true;
                }
                b"--append" => {
                    if append_to_output_path {
                        return Err(OutputTypeError::ArgTwice("--append"));
                    }
                    if allow_stdout {
                        return Err(OutputTypeError::ArgWith("--append", "--stdout".to_string()));
                    }
                    append_to_output_path = true;
                }
                b"-o" | b"--output-file" => {
                    let new_path = argv
                        .pop_front()
                        .map(PathBuf::from)
                        .ok_or_else(|| OutputTypeError::NoValFor("-o/--output-file"))?;
                    if let Some(prev_path) = output_path.take() {
                        return Err(OutputTypeError::ValArgTwice {
                            arg: "-o/--output-file",
                            prev: format!("{prev_path:?}"),
                            new: format!("{new_path:?}"),
                        });
                    }
                    if allow_stdout {
                        return Err(OutputTypeError::ArgWith(
                            "--stdout",
                            "-o/--output-file".to_string(),
                        ));
                    }
                    output_path = Some(new_path);
                }
                _ => {
                    argv.push_front(arg);
                    break;
                }
            }
        }

        Ok(if let Some(output_path) = output_path {
            Self::File {
                path: output_path,
                append: append_to_output_path,
            }
        } else {
            Self::Stdout {
                allow_tty: allow_stdout,
            }
        })
    }
}

#[cfg(feature = "json")]
pub mod json_resource {
    use super::*;
    use crate::schema::backends::{json_backend::JsonBackend, Backend};

    use std::{error, fmt};

    use json::{object::Object as JsonObject, JsonValue};

    #[derive(Debug)]
    pub enum JsonOutputTypeError {
        InvalidType {
            val: JsonValue,
            valid_types: &'static [&'static str],
            context: &'static str,
        },
        InvalidObjectKeys {
            obj: JsonObject,
            expected_keys: &'static [&'static str],
            context: &'static str,
        },
    }

    impl fmt::Display for JsonOutputTypeError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Self::InvalidType {
                    valid_types,
                    context,
                    val,
                } => {
                    assert!(!valid_types.is_empty());
                    let types_str: String = valid_types.join(", ");
                    write!(
                        f,
                        "{context} expected types [{types_str}], but received: {val}"
                    )
                }
                Self::InvalidObjectKeys {
                    obj,
                    expected_keys,
                    context,
                } => {
                    assert!(!expected_keys.is_empty());
                    let keys_str: String = expected_keys.join(", ");
                    let obj = JsonValue::Object(obj.clone());
                    write!(
                        f,
                        "{context} expected object keys [{keys_str}], but object was {obj}"
                    )
                }
            }
        }
    }

    impl error::Error for JsonOutputTypeError {}

    impl SchemaResource for OutputType {
        type B = JsonBackend;
        type SchemaParseError = JsonOutputTypeError;

        fn parse_schema<'a>(
            v: <Self::B as Backend>::Val<'a>,
        ) -> Result<Self, Self::SchemaParseError> {
            match v {
                /* <string> => {"file": {"path": <string>, "append": false}}} */
                JsonValue::String(path) => Ok(OutputType::File {
                    path: path.into(),
                    append: false,
                }),
                /* <bool> => {"stdout": {"allow_tty": <bool>}} */
                JsonValue::Boolean(allow_tty) => Ok(OutputType::Stdout { allow_tty }),
                /* An object--destructure by enum case. */
                JsonValue::Object(o) => {
                    if let Some(o) = o.get("stdout") {
                        match o {
                            /* {"stdout": <bool>} => {"stdout": {"allow_tty": <bool>}} */
                            JsonValue::Boolean(allow_tty) => Ok(OutputType::Stdout {
                                allow_tty: *allow_tty,
                            }),
                            /* {"stdout": {"allow_tty": <bool>}} => {"stdout": {"allow_tty": <bool>}} */
                            JsonValue::Object(o) => {
                                let allow_tty: bool = if let Some(allow_tty) = o.get("allow_tty") {
                                    match allow_tty {
                                        JsonValue::Boolean(allow_tty) => Ok(*allow_tty),
                                        _ => Err(JsonOutputTypeError::InvalidType {
                                            val: allow_tty.clone(),
                                            valid_types: &["boolean"],
                                            context: "the 'allow_tty' field in the 'stdout' case",
                                        }),
                                    }
                                } else {
                                    Ok(false)
                                }?;
                                Ok(OutputType::Stdout { allow_tty })
                            }
                            _ => Err(JsonOutputTypeError::InvalidType {
                                val: o.clone(),
                                valid_types: &["boolean", "object"],
                                context: "the 'stdout' enum case",
                            }),
                        }
                    } else if let Some(o) = o.get("file") {
                        match o {
                            /* {"file": <string>} => {"file": {"path": <string>, append: false}} */
                            JsonValue::String(path) => Ok(OutputType::File {
                                path: path.into(),
                                append: false,
                            }),
                            /* {"file": {"path": <string>, "append": <bool>}} => {"file": {"path": <string>, append: <bool>}} */
                            JsonValue::Object(o) => {
                                let path: PathBuf = if let Some(path) = o.get("path") {
                                    match path {
                                        JsonValue::String(path) => Ok(path.into()),
                                        _ => Err(JsonOutputTypeError::InvalidType {
                                            val: path.clone(),
                                            valid_types: &["string"],
                                            context: "the 'path' field in the 'file' case",
                                        }),
                                    }
                                } else {
                                    /* This *must* be provided, whereas "append" has a default. */
                                    Err(JsonOutputTypeError::InvalidObjectKeys {
                                        obj: o.clone(),
                                        expected_keys: &["path"],
                                        context: "the 'file' enum case",
                                    })
                                }?;
                                let append: bool = if let Some(append) = o.get("append") {
                                    match append {
                                        JsonValue::Boolean(append) => Ok(*append),
                                        _ => Err(JsonOutputTypeError::InvalidType {
                                            val: append.clone(),
                                            valid_types: &["boolean"],
                                            context: "the 'append' field in 'file' case",
                                        }),
                                    }
                                } else {
                                    Ok(false)
                                }?;
                                Ok(OutputType::File { path, append })
                            }
                            _ => Err(JsonOutputTypeError::InvalidType {
                                val: o.clone(),
                                valid_types: &["string", "object"],
                                context: "the 'file' enum case",
                            }),
                        }
                    } else {
                        Err(JsonOutputTypeError::InvalidObjectKeys {
                            obj: o,
                            expected_keys: &["stdout", "file"],
                            context: "destructuring into 'file' and 'stdout' enum cases",
                        })
                    }
                }
                _ => Err(JsonOutputTypeError::InvalidType {
                    val: v,
                    valid_types: &["string", "boolean", "object"],
                    context: "top-level value",
                }),
            }
        }
    }
}
