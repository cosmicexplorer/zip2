use super::backends::Backend;

/* pub trait SchemaValue<B: Backend>: Sized { */
/*     type DeserErr; */
/*     fn serialize(self) -> <B as Backend>::Value; */
/*     fn deserialize(s: <B as Backend>::Value) -> Result<Self, Self::DeserErr>; */
/* } */

pub trait NamedList {
    fn f(self);
}

/* pub enum Schema { */
/*     Bool, */
/*     Str, */
/*     Arr, */
/*     Obj, */
/*     Arr(Vec<Box<Schema>>) */
/*     Str(String), */
/*     Arr(Vec<Box<Schema>>), */
/*     Obj(Vec<(String, Box<Schema>)>), */
/* } */

pub enum HydratedValue<'a> {
    Bool(bool),
    Str(&'a str),
    Arr(Vec<Box<HydratedValue<'a>>>),
    Obj(Vec<(&'a str, Box<HydratedValue<'a>>)>),
}

pub trait Hydrate<Value> {
    fn hydrate(v: HydratedValue) -> Value;
}

pub trait Schema: Backend {
    fn print<'a>(v: HydratedValue<'a>) -> <Self as Backend>::Val<'a>;
}

#[cfg(feature = "json")]
pub mod json_value {
    use super::*;
    use crate::schema::backends::json_backend::JsonBackend;

    /* impl SchemaValue<JsonBackend> for bool { */
    /*     type DeserErr = String; */

    /*     fn serialize(self) -> json::JsonValue { */
    /*         json::JsonValue::Boolean(self) */
    /*     } */
    /*     fn deserialize(s: json::JsonValue) -> Result<Self, String> { */
    /*         match s { */
    /*             json::JsonValue::Boolean(value) => Ok(value), */
    /*             s => Err(format!("non-boolean value {s}")), */
    /*         } */
    /*     } */
    /* } */

    /* impl SchemaValue<JsonBackend> for String { */
    /*     type DeserErr = String; */

    /*     fn serialize(self) -> json::JsonValue { */
    /*         json::JsonValue::String(self) */
    /*     } */
    /*     fn deserialize(s: json::JsonValue) -> Result<Self, String> { */
    /*         match s { */
    /*             json::JsonValue::String(value) => Ok(value), */
    /*             s => Err(format!("non-string value {s}")), */
    /*         } */
    /*     } */
    /* } */
}

/* pub enum SchemaValue { */
/*     Bool(bool), */
/*     Path(PathBuf), */
/* } */

/* pub trait SchemaValue<B: Backend> {} */

/* impl SchemaValue for bool {} */
