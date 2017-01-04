use rustler;
use rustler::{NifEnv, NifTerm, NifEncoder, NifResult};

pub fn add_u32<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let lhs: u32 = try!(args[0].decode());
    let rhs: u32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}
pub fn add_i32<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let lhs: i32 = try!(args[0].decode());
    let rhs: i32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}

#[derive(TupleTermTarget)]
struct AddTuple {
    lhs: i32,
    rhs: i32,
}

pub fn tuple_add<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let tuple: AddTuple = try!(args[0].decode());
    Ok((tuple.lhs + tuple.rhs).encode(env))
}

pub fn echo_u8<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let num: u8 = try!(args[0].decode());
    Ok(num.encode(env))
}
