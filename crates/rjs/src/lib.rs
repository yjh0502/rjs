use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use rustler::types::{atom::Atom, Binary, MapIterator, OwnedBinary};
use rustler::Error::BadArg;
use rustler::{Encoder, Env, NifResult, Term, TermType};
use serde::de::{DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor};
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

mod atoms {
    rustler::atoms! {
        ok,
        t = "true",
        f = "false",
        null,
        undefined,

        labels,
        binary,
        atom,
        attempt_atom,
        existing_atom,
    }
}

#[derive(Clone, Copy)]
enum LabelOpt {
    Binary,
    Atom,
    ExistingAtom,
    AttemptAtom,
}

impl Default for LabelOpt {
    fn default() -> Self {
        LabelOpt::Binary
    }
}

#[derive(Clone, Copy, Default)]
struct DecodeOpt {
    label: LabelOpt,
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
                let atom = rustler::Atom::from_term(term).map_err(badarg)?;
                if atom == atoms::null() {
                    serializer.serialize_unit()
                } else if atom == atoms::t() {
                    serializer.serialize_bool(true)
                } else if atom == atoms::f() {
                    serializer.serialize_bool(false)
                } else {
                    let s = term.atom_to_string().map_err(badarg)?;
                    serializer.serialize_str(&s)
                }
            }

            Binary => {
                let bin = term.decode_as_binary().map_err(badarg)?;
                let s = std::str::from_utf8(&bin).map_err(badarg)?;
                serializer.serialize_str(s)
            }

            EmptyList => {
                let seq = serializer.serialize_seq(Some(0))?;
                seq.end()
            }

            List => {
                let mut tail = term;

                let mut seq = serializer.serialize_seq(None)?;
                while !tail.is_empty_list() {
                    let (head, next_tail) = tail.list_get_cell().map_err(badarg)?;

                    let elem = MyTerm::from(head);
                    seq.serialize_element(&elem)?;

                    tail = next_tail;
                }
                seq.end()
            }

            Map => {
                let size = term.map_size().map_err(badarg)?;
                let iter = MapIterator::new(term).ok_or_else(|| badarg(()))?;
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

struct MapKeyVisitor<'a, 'b> {
    v: &'b TermVisitor<'a>,
}
impl<'a, 'b> From<&'b TermVisitor<'a>> for MapKeyVisitor<'a, 'b> {
    fn from(v: &'b TermVisitor<'a>) -> Self {
        Self { v }
    }
}

impl<'de, 'a, 'b> DeserializeSeed<'de> for MapKeyVisitor<'a, 'b> {
    type Value = Term<'a>;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'de, 'a, 'b> Visitor<'de> for MapKeyVisitor<'a, 'b> {
    type Value = Term<'a>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.v.get_key(value).map_err(dbadarg)
    }
}

struct TermVisitor<'a> {
    env: Env<'a>,
    opt: DecodeOpt,
    key_cache: RefCell<HashMap<String, Term<'a>>>,
}

impl<'a> TermVisitor<'a> {
    fn get_key(&self, value: &str) -> Result<Term<'a>, rustler::Error> {
        let mut m = self.key_cache.borrow_mut();
        if let Some(v) = m.get(value) {
            return Ok(v.clone());
        }

        let s = value.to_owned();

        let key = match self.opt.label {
            LabelOpt::Atom => Atom::from_str(self.env, value)?.to_term(self.env),

            LabelOpt::Binary => value.encode(self.env),

            LabelOpt::AttemptAtom => {
                //
                match Atom::try_from_bytes(self.env, value.as_bytes())? {
                    Some(atom) => atom.to_term(self.env),
                    None => return Err(rustler::Error::BadArg),
                }
            }

            LabelOpt::ExistingAtom => {
                //
                match Atom::try_from_bytes(self.env, value.as_bytes())? {
                    Some(atom) => atom.to_term(self.env),
                    None => value.encode(self.env),
                }
            }
        };

        m.insert(s, key);
        Ok(key)
    }
}

impl<'de, 'a, 'b> DeserializeSeed<'de> for &'b TermVisitor<'a> {
    type Value = Term<'a>;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'de, 'a, 'b> Visitor<'de> for &'b TermVisitor<'a> {
    type Value = Term<'a>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        let atom = if value { atoms::t() } else { atoms::f() };
        Ok(atom.to_term(self.env))
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
        let mut vec = vec_maybe_size(visitor.size_hint());
        while let Some(elem) = visitor.next_element_seed(self)? {
            vec.push(elem);
        }
        Ok(vec.encode(self.env))
    }

    #[inline]
    fn visit_map<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut keys = vec_maybe_size(visitor.size_hint());
        let mut values = vec_maybe_size(visitor.size_hint());

        loop {
            let key = visitor.next_key_seed(MapKeyVisitor::from(self))?;
            match key {
                Some(key) => {
                    let value = visitor.next_value_seed(self)?;
                    keys.push(key);
                    values.push(value);
                }
                None => break,
            }
        }

        Term::map_from_arrays(self.env, &keys, &values).map_err(dbadarg)
    }
}

fn vec_maybe_size<T>(size: Option<usize>) -> Vec<T> {
    match size {
        Some(size) => Vec::with_capacity(size),
        None => Vec::new(),
    }
}

rustler::init!("rjs", [nif_encode, nif_decode]);

#[rustler::nif]
fn nif_encode<'a>(env: Env<'a>, term: Term<'a>) -> NifResult<Term<'a>> {
    let term = MyTerm::from(term);
    let out = serde_json::to_string(&term).map_err(|_e| BadArg)?;

    let mut bin = OwnedBinary::new(out.len()).ok_or(BadArg)?;
    (&mut bin).copy_from_slice(out.as_bytes());

    let bin = Binary::from_owned(bin, env);
    Ok((atoms::ok(), bin.to_term(env)).encode(env))
}

fn parse_decode_opts<'a>(env: Env<'a>, mut t: Term<'a>) -> NifResult<DecodeOpt> {
    let mut opt = DecodeOpt {
        label: LabelOpt::Binary,
    };
    while !t.is_empty_list() {
        let (head, next_tail) = t.list_get_cell()?;
        t = next_tail;

        if head == atoms::binary().to_term(env) {
            opt.label = LabelOpt::Binary;
        }
        if head == atoms::atom().to_term(env) {
            opt.label = LabelOpt::Atom;
        }
        if head == atoms::existing_atom().to_term(env) {
            opt.label = LabelOpt::ExistingAtom;
        }
        if head == atoms::attempt_atom().to_term(env) {
            opt.label = LabelOpt::AttemptAtom;
        }
    }

    Ok(opt)
}

#[rustler::nif]
fn nif_decode<'a>(env: Env<'a>, data: Binary<'a>, opts: Term<'a>) -> NifResult<Term<'a>> {
    let s = std::str::from_utf8(&data).map_err(|_e| BadArg)?;

    let opt = parse_decode_opts(env, opts)?;

    let read = serde_json::de::StrRead::new(s);
    let mut deser = serde_json::de::Deserializer::new(read);

    let seed = TermVisitor {
        env,
        opt,
        key_cache: Default::default(),
    };
    let res = seed.deserialize(&mut deser).map_err(|_e| BadArg)?;

    Ok((atoms::ok(), res).encode(env))
}
