#[macro_use]
extern crate rustler;

pub type NifEnv = *mut rustler_sys::rustler_sys_api::ErlNifEnv;

use std::cell::Cell;
use std::fmt;

use rustler::types::{atom::Atom, Binary, MapIterator, OwnedBinary};
use rustler::Error::BadArg;
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

#[derive(Clone, Copy)]
struct DecodeOpt {
    label_atom: bool,
}

#[derive(Clone, Copy)]
struct DecodeEnv {
    nifenv: NifEnv,
    opt: DecodeOpt,
}

thread_local!(static ENV: Cell<Option<DecodeEnv>> = Cell::new(None));

type MapKeyTerm<'a> = VTerm<'a, MapKeyVisitor<'a>>;
type MyTerm<'a> = VTerm<'a, TermVisitor<'a>>;

struct VTerm<'a, V> {
    inner: Term<'a>,
    _unused: std::marker::PhantomData<V>,
}
impl<'a, V> Into<Term<'a>> for VTerm<'a, V> {
    fn into(self) -> Term<'a> {
        self.inner
    }
}
impl<'a, V> From<Term<'a>> for VTerm<'a, V> {
    fn from(inner: Term<'a>) -> Self {
        Self {
            inner,
            _unused: std::default::Default::default(),
        }
    }
}

impl<'de, V> Deserialize<'de> for VTerm<'de, V>
where
    V: Visitor<'de, Value = Term<'de>> + EnvVisitor<'de>,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        ENV.with(|env| {
            let env = env
                .get()
                .expect("deserialize should be called with env context");
            let inner = {
                let env0 = unsafe { Env::new(&ENV, env.nifenv) };
                let visitor = V::from_env(env0, env.opt);
                deserializer.deserialize_any(visitor)?
            };
            Ok(inner.into())
        })
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

impl<'a> EnvVisitor<'a> for MapKeyVisitor<'a> {
    fn from_env(env: Env<'a>, _opt: DecodeOpt) -> Self {
        Self { env }
    }
}

impl<'de> Visitor<'de> for MapKeyVisitor<'de> {
    type Value = Term<'de>;

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

impl<'a> EnvVisitor<'a> for TermVisitor<'a> {
    fn from_env(env: Env<'a>, opt: DecodeOpt) -> Self {
        Self { env, opt }
    }
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

    fn visit_map<V>(self, visitor: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        if self.opt.label_atom {
            build_map::<MapKeyTerm, _>(self.env, visitor)
        } else {
            build_map::<MyTerm, _>(self.env, visitor)
        }
    }
}

fn build_map<'a, T, V>(env: Env<'a>, mut visitor: V) -> Result<Term<'a>, V::Error>
where
    V: MapAccess<'a>,
    T: Deserialize<'a> + Into<Term<'a>>,
{
    let mut map = Term::map_new(env);
    while let Some((key, value)) = visitor.next_entry::<T, MyTerm>()? {
        map = map.map_put(key.into(), value.into()).map_err(dbadarg)?;
    }
    Ok(map)
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

    let res = ENV.with(|c| {
        let env = DecodeEnv {
            nifenv: env.as_c_arg(),
            opt: DecodeOpt { label_atom: false },
        };
        c.set(Some(env));
        let res = serde_json::from_str::<MyTerm>(s).map_err(|_e| BadArg);
        c.set(None);
        res
    });

    Ok((atoms::ok(), res?.inner).encode(env))
}
