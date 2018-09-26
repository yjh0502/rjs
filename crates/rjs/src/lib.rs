#[macro_use]
extern crate rustler;
#[macro_use]
extern crate lazy_static;
extern crate erlang_nif_sys;
extern crate serde;
extern crate serde_json;

pub type NifEnv = *mut erlang_nif_sys::ErlNifEnv;

use std::cell::Cell;
use std::fmt;

use rustler::types::{Binary, MapIterator, OwnedBinary};
use rustler::{Encoder, Env, NifResult, Term, TermType};
use serde::de::{Deserialize, MapAccess, SeqAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

mod atoms {
    rustler_atoms! {
        atom ok;
        atom t = "true";
        atom f = "false";
        atom null;
        atom undefined;
    }
}

thread_local!(static ENV: Cell<Option<NifEnv>> = Cell::new(None));

struct MyTerm<'a> {
    inner: Term<'a>,
}
impl<'de> Deserialize<'de> for MyTerm<'de> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        ENV.with(|env| {
            let env = env
                .get()
                .expect("deserialize should be done with env context");
            let inner = {
                let env = unsafe { Env::new(&ENV, env) };
                let visitor = TermVisitor { env };
                deserializer.deserialize_any(visitor)?
            };
            Ok(Self { inner })
        })
    }
}

fn badarg<E1, E2>(_e: E1) -> E2
where
    E2: serde::ser::Error,
{
    E2::custom("badarg")
}

impl<'a> Serialize for MyTerm<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use TermType::*;
        let term = self.inner;

        match term.get_type() {
            Atom => {
                let s = term.atom_to_string().map_err(badarg)?;

                serializer.serialize_str(&s)
            }

            Binary => {
                let bin = term.into_binary().map_err(badarg)?;
                let s = std::str::from_utf8(bin.as_slice()).map_err(badarg)?;
                serializer.serialize_str(s)
            }

            List => {
                let mut v = Vec::new();

                let mut tail = term;
                while !tail.is_empty_list() {
                    let (head, next_tail) = tail.list_get_cell().map_err(badarg)?;
                    v.push(head);
                    tail = next_tail;
                }

                let mut seq = serializer.serialize_seq(Some(v.len()))?;
                for elem in v {
                    seq.serialize_element(&MyTerm { inner: elem })?;
                }
                seq.end()
            }

            Map => {
                let size = term.map_size().map_err(badarg)?;
                let iter = match MapIterator::new(term) {
                    Some(iter) => iter,
                    None => return Err(badarg(())),
                };
                let mut map = serializer.serialize_map(Some(size))?;
                for (key, val) in iter {
                    map.serialize_entry(&MyTerm { inner: key }, &MyTerm { inner: val })?;
                }
                map.end()
            }

            Number => match term.decode::<f64>() {
                Ok(v) => serializer.serialize_f64(v),
                Err(_e) => {
                    let v = term.decode::<i64>().map_err(badarg)?;
                    serializer.serialize_i64(v)
                }
            },

            Tuple | EmptyList | Exception | Fun | Pid | Port | Ref | Unknown => Err(badarg(())),
        }
    }
}

#[derive(Clone, Copy)]
struct TermVisitor<'a> {
    env: Env<'a>,
}

impl<'de> Visitor<'de> for TermVisitor<'de> {
    type Value = Term<'de>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        if value {
            Ok(atoms::t().to_term(self.env))
        } else {
            Ok(atoms::f().to_term(self.env))
        }
    }

    #[inline]
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        Ok(value.encode(self.env))
    }

    #[inline]
    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        Ok(value.encode(self.env))
    }

    #[inline]
    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E> {
        Ok(value.encode(self.env))
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(value.encode(self.env))
    }

    #[inline]
    fn visit_string<E>(self, value: String) -> Result<Self::Value, E> {
        Ok(value.encode(self.env))
    }

    #[inline]
    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(atoms::undefined().to_term(self.env))
    }

    #[inline]
    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(atoms::null().to_term(self.env))
    }

    #[inline]
    fn visit_seq<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
    where
        V: SeqAccess<'de>,
    {
        let mut vec = Vec::new();
        while let Some(elem) = visitor.next_element::<MyTerm>()? {
            vec.push(elem.inner);
        }
        let mut term = Term::list_new_empty(self.env);
        for item in vec.into_iter().rev() {
            term = term.list_prepend(item);
        }

        Ok(term)
    }

    fn visit_map<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        use serde::de::Error;

        let mut map = Term::map_new(self.env);
        while let Some((key, value)) = visitor.next_entry::<MyTerm, MyTerm>()? {
            map = map
                .map_put(key.inner, value.inner)
                .map_err(|_e| V::Error::custom("badarg"))?;
        }
        Ok(map)
    }
}

rustler_export_nifs!(
    "rjs",
    [("nif_encode", 1, encode), ("nif_decode", 1, decode)],
    None
);

fn encode<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let term = args[0];
    let out =
        serde_json::to_string(&MyTerm { inner: term }).map_err(|_e| rustler::Error::BadArg)?;

    let mut bin = match OwnedBinary::new(out.len()) {
        Some(bin) => bin,
        None => return Err(rustler::Error::BadArg),
    };
    bin.as_mut_slice().copy_from_slice(out.as_bytes());

    let bin = Binary::from_owned(bin, env);
    Ok((atoms::ok(), bin.to_term(env)).encode(env))
}

fn decode<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let data = Binary::from_term(args[0])?;
    let s = std::str::from_utf8(data.as_slice()).map_err(|_e| rustler::Error::BadArg)?;

    let res = ENV.with(|c| {
        c.set(Some(env.as_c_arg()));
        let res = serde_json::from_str::<MyTerm>(s).map_err(|_e| rustler::Error::BadArg);
        c.set(None);
        res
    });

    Ok((atoms::ok(), res?.inner).encode(env))
}
