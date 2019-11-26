#[macro_use]
extern crate rustler;

use std::fmt;

use rustler::types::{atom::Atom, Binary, MapIterator, OwnedBinary};
use rustler::Error::BadArg;
use rustler::{Encoder, Env, NifResult, Term, TermType};
use serde::de::{DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor};
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

#[derive(Clone, Copy)]
struct DecodeOpt {
    label_atom: bool,
}

struct MyTerm<'a> {
    inner: Term<'a>,
}
impl<'a> Into<Term<'a>> for MyTerm<'a> {
    fn into(self) -> Term<'a> {
        self.inner
    }
}
impl<'a> From<Term<'a>> for MyTerm<'a> {
    fn from(inner: Term<'a>) -> Self {
        Self { inner }
    }
}

fn badarg<E1, E2>(_e: E1) -> E2
where
    E2: serde::ser::Error,
{
    E2::custom("badarg")
}

fn dbadarg<E1, E2>(_e: E1) -> E2
where
    E2: serde::de::Error,
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
                let s = std::str::from_utf8(&bin).map_err(badarg)?;
                serializer.serialize_str(s)
            }

            EmptyList => {
                let seq = serializer.serialize_seq(Some(0))?;
                seq.end()
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
                    let t_elem = MyTerm::from(elem);
                    seq.serialize_element(&t_elem)?;
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
                    let t_key = MyTerm::from(key);
                    let t_val = MyTerm::from(val);
                    map.serialize_entry(&t_key, &t_val)?;
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

            Tuple | Exception | Fun | Pid | Port | Ref | Unknown => Err(badarg(())),
        }
    }
}

#[allow(unused)]
enum Labels {
    Binary,
}

trait EnvVisitor<'a> {
    fn from_env(env: Env<'a>, opt: DecodeOpt) -> Self;
}

#[derive(Clone, Copy)]
struct MapKeyVisitor<'a> {
    env: Env<'a>,
}
impl<'a> From<TermVisitor<'a>> for MapKeyVisitor<'a> {
    fn from(v: TermVisitor<'a>) -> Self {
        Self { env: v.env }
    }
}

impl<'de, 'a> DeserializeSeed<'de> for MapKeyVisitor<'a> {
    type Value = Term<'a>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self.clone())
    }
}

impl<'a> EnvVisitor<'a> for MapKeyVisitor<'a> {
    fn from_env(env: Env<'a>, _opt: DecodeOpt) -> Self {
        Self { env }
    }
}

impl<'de, 'a> Visitor<'de> for MapKeyVisitor<'a> {
    type Value = Term<'a>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let atom = Atom::from_str(self.env, value).map_err(dbadarg)?;
        Ok(atom.to_term(self.env))
    }
}

#[derive(Clone, Copy)]
struct TermVisitor<'a> {
    env: Env<'a>,
    opt: DecodeOpt,
}

impl<'de, 'a> DeserializeSeed<'de> for TermVisitor<'a> {
    type Value = Term<'a>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'a> EnvVisitor<'a> for TermVisitor<'a> {
    fn from_env(env: Env<'a>, opt: DecodeOpt) -> Self {
        Self { env, opt }
    }
}

impl<'de, 'a> Visitor<'de> for TermVisitor<'a> {
    type Value = Term<'a>;

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
        D: Deserializer<'de>,
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
        let mut vec = match visitor.size_hint() {
            Some(size) => Vec::with_capacity(size),
            None => Vec::new(),
        };

        while let Some(elem) = visitor.next_element_seed(self)? {
            vec.push(elem);
        }
        Ok(vec.as_slice().encode(self.env))
    }

    #[inline]
    fn visit_map<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut map = Term::map_new(self.env);
        loop {
            let res = if self.opt.label_atom {
                visitor.next_entry_seed(MapKeyVisitor::from(self), self)?
            } else {
                visitor.next_entry_seed(self, self)?
            };

            match res {
                Some((key, value)) => {
                    map = map.map_put(key.into(), value.into()).map_err(dbadarg)?;
                }
                None => return Ok(map),
            }
        }
    }
}

rustler_export_nifs!(
    "rjs",
    [("nif_encode", 1, encode), ("nif_decode", 1, decode)],
    None
);

fn encode<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    if args.len() != 1 {
        return Err(BadArg);
    }

    let term = args[0];
    let term = MyTerm::from(term);
    let out = serde_json::to_string(&term).map_err(|_e| BadArg)?;

    let mut bin = OwnedBinary::new(out.len()).ok_or(BadArg)?;
    (&mut bin).copy_from_slice(out.as_bytes());

    let bin = Binary::from_owned(bin, env);
    Ok((atoms::ok(), bin.to_term(env)).encode(env))
}

fn decode<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    if args.len() != 1 {
        return Err(BadArg);
    }

    let data = Binary::from_term(args[0])?;
    let s = std::str::from_utf8(&data).map_err(|_e| BadArg)?;
    let opt = DecodeOpt { label_atom: false };

    let seed = TermVisitor { env, opt };

    let read = serde_json::de::StrRead::new(s);
    let res = seed
        .deserialize(&mut serde_json::de::Deserializer::new(read))
        .map_err(|_e| BadArg)?;

    Ok((atoms::ok(), res).encode(env))
}
