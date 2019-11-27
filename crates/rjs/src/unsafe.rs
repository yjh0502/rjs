use super::*;
use std::mem::MaybeUninit;

#[derive(Clone, Copy)]
pub struct MapKeyVisitor<'a> {
    env: Env<'a>,
}
impl<'a> From<TermVisitor<'a>> for MapKeyVisitor<'a> {
    fn from(v: TermVisitor<'a>) -> Self {
        Self { env: v.env }
    }
}

impl<'de, 'a> DeserializeSeed<'de> for MapKeyVisitor<'a> {
    type Value = rustler_sys::ERL_NIF_TERM;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

impl<'de, 'a> Visitor<'de> for MapKeyVisitor<'a> {
    type Value = rustler_sys::ERL_NIF_TERM;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let atom = Atom::from_str(self.env, value).map_err(dbadarg)?;
        Ok(atom.to_term(self.env).as_c_arg())
    }
}

#[derive(Clone, Copy)]
pub struct TermVisitor<'a> {
    pub env: Env<'a>,
    pub(crate) opt: DecodeOpt,
}

impl<'de, 'a> DeserializeSeed<'de> for TermVisitor<'a> {
    type Value = rustler_sys::ERL_NIF_TERM;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
}

use rustler_sys::rustler_sys_api::*;

impl<'de, 'a> Visitor<'de> for TermVisitor<'a> {
    type Value = rustler_sys::ERL_NIF_TERM;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
    }

    #[inline]
    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        let atom = if value { atoms::t() } else { atoms::f() };
        Ok(atom.as_c_arg())
    }

    #[inline]
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        unsafe { Ok(enif_make_int64(self.env.as_c_arg(), value)) }
    }

    #[inline]
    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        unsafe { Ok(enif_make_uint64(self.env.as_c_arg(), value)) }
    }

    #[inline]
    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E> {
        unsafe { Ok(enif_make_double(self.env.as_c_arg(), value)) }
    }

    #[inline]
    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(value.encode(self.env).as_c_arg())
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
        unsafe {
            Ok(enif_make_list_from_array(
                self.env.as_c_arg(),
                vec.as_ptr(),
                vec.len() as u32,
            ))
        }
    }

    #[inline]
    fn visit_map<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
    where
        V: MapAccess<'de>,
    {
        let mut keys = vec_maybe_size(visitor.size_hint());
        let mut values = vec_maybe_size(visitor.size_hint());

        loop {
            let key = if self.opt.label_atom {
                visitor.next_key_seed(MapKeyVisitor::from(self))?
            } else {
                visitor.next_key_seed(self)?
            };
            match key {
                Some(key) => {
                    let value = visitor.next_value_seed(self)?;
                    keys.push(key);
                    values.push(value);
                }
                None => break,
            }
        }

        unsafe {
            let mut map = MaybeUninit::uninit();
            if rustler_sys::enif_make_map_from_arrays(
                self.env.as_c_arg(),
                keys.as_ptr(),
                values.as_ptr(),
                keys.len() as usize,
                map.as_mut_ptr(),
            ) == 0
            {
                unimplemented!();
            }
            Ok(map.assume_init())
        }
    }
}

fn vec_maybe_size<T>(size: Option<usize>) -> Vec<T> {
    match size {
        Some(size) => Vec::with_capacity(size),
        None => Vec::new(),
    }
}
