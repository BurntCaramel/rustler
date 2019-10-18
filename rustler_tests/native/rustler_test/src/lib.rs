#![allow(deprecated)]
use rustler::schedule::SchedulerFlags::*;
use rustler::{Env, Term};

#[cfg(test)]
mod test {
    #![allow(dead_code)]

    #[test]
    fn test_nif_macro() {
        pub mod math {
            #[rustler::nif(schedule = "DirtyCpu")]
            fn add(a: i64, b: i64) -> i64 {
                a + b
            }

            #[rustler::nif(name = "sub2")]
            fn sub(a: u32, b: u32) -> Result<u32, rustler::Error> {
                Ok(a - b)
            }
        }

        fn load(_env: rustler::Env, _: rustler::Term) -> bool {
            true
        }

        rustler::init!("math", [math::add, math::sub], Some(load));
    }

    //#[cfg(test)]
    //fn test_nif_macro() {
    //pub mod math {
    //pub mod sub {
    //pub const NAME: *const u8 = concat!("sub", "\0").as_ptr() as *const u8;
    //pub const ARITY: u32 = 0u32;
    //pub const FLAGS: u32 = rustler::SchedulerFlags::Normal as u32;
    //pub const FUNC: unsafe extern "C" fn(
    //nif_env: rustler::codegen_runtime::NIF_ENV,
    //argc: rustler::codegen_runtime::c_int,
    //argv: *const rustler::codegen_runtime::NIF_TERM,
    //)
    //-> rustler::codegen_runtime::NIF_TERM = {
    //unsafe extern "C" fn nif_func(
    //nif_env: rustler::codegen_runtime::NIF_ENV,
    //argc: rustler::codegen_runtime::c_int,
    //argv: *const rustler::codegen_runtime::NIF_TERM,
    //) -> rustler::codegen_runtime::NIF_TERM {
    //let lifetime = ();
    //let env = rustler::Env::new(&lifetime, nif_env);
    //let terms = std::slice::from_raw_parts(argv, argc as usize)
    //.iter()
    //.map(|term| rustler::Term::new(env, *term))
    //.collect::<Vec<rustler::Term>>();
    //fn wrapper<'a>(
    //env: rustler::Env<'a>,
    //args: &[rustler::Term<'a>],
    //) -> rustler::codegen_runtime::NifReturned {
    //let result = std::panic::catch_unwind(move || {
    //let a: u32 = args[0usize]
    //.decode()
    //.map_err(|_| "unsupported function argument type `u32` for `a`")
    //.expect("unsupported function argument type `u32` for `a`");
    //let b: u32 = args[1usize]
    //.decode()
    //.map_err(|_| "unsupported function argument type `u32` for `b`")
    //.expect("unsupported function argument type `u32` for `b`");
    //fn sub(a: u32, b: u32) -> Result<u32, rustler::Error> {
    //Ok(a - b)
    //}
    //sub(a, b)
    //});
    //unsafe {
    //match result {
    //Ok(res) => {
    //rustler::codegen_runtime::NifReturnable::as_returned(
    //res, env,
    //)
    //}
    //Err(_err) => {
    //let error: rustler::Error =
    //rustler::Error::Atom("nif_panic");
    //rustler::codegen_runtime::NifReturnable::as_returned(
    //error, env,
    //)
    //}
    //}
    //}
    //}
    //wrapper(env, &terms).apply(env)
    //}
    //nif_func
    //};
    //pub const DEF_NIF_FUNC: rustler::codegen_runtime::DEF_NIF_FUNC =
    //rustler::codegen_runtime::DEF_NIF_FUNC {
    //arity: ARITY,
    //flags: FLAGS,
    //function: FUNC,
    //name: NAME,
    //};
    //}
    //}
    //fn load(_env: rustler::Env, _: rustler::Term) -> bool {
    //true
    //}
    //#[cfg(unix)]
    //#[no_mangle]
    //pub extern "C" fn nif_init() -> *const rustler::codegen_runtime::DEF_NIF_ENTRY {
    //static mut NIF_ENTRY: Option<rustler::codegen_runtime::DEF_NIF_ENTRY> = None;
    //const FUN_ENTRIES: &'static [rustler::codegen_runtime::DEF_NIF_FUNC] =
    //&[math::sub::DEF_NIF_FUNC];
    //let entry = rustler::codegen_runtime::DEF_NIF_ENTRY {
    //major: rustler::codegen_runtime::NIF_MAJOR_VERSION,
    //minor: rustler::codegen_runtime::NIF_MINOR_VERSION,
    //name: (std::ffi::CString::new("math")
    //.expect("CString::new")
    //.as_ptr() as *const u8),
    //num_of_funcs: 1usize as rustler::codegen_runtime::c_int,
    //funcs: FUN_ENTRIES.as_ptr(),
    //load: {
    //extern "C" fn nif_load(
    //env: rustler::codegen_runtime::NIF_ENV,
    //_priv_data: *mut *mut rustler::codegen_runtime::c_void,
    //load_info: rustler::codegen_runtime::NIF_TERM,
    //) -> rustler::codegen_runtime::c_int {
    //unsafe {
    //rustler::codegen_runtime::handle_nif_init_call(
    //Some(load),
    //env,
    //load_info,
    //)
    //}
    //}
    //Some(nif_load)
    //},
    //reload: None,
    //upgrade: None,
    //unload: None,
    //vm_variant: (std::ffi::CString::new("beam.vanilla").unwrap().as_ptr() as *const u8),
    //options: 0,
    //sizeof_ErlNifResourceTypeInit:
    //rustler::codegen_runtime::get_nif_resource_type_init_size(),
    //};

    //unsafe {
    //NIF_ENTRY = Some(entry);
    //NIF_ENTRY.as_ref().unwrap()
    //}
    //}
    //}
}

mod test_atom;
mod test_binary;
mod test_codegen;
mod test_dirty;
mod test_env;
mod test_list;
mod test_map;
mod test_primitives;
mod test_range;
mod test_resource;
mod test_term;
mod test_thread;

rustler::rustler_export_nifs!(
    "Elixir.RustlerTest",
    [
        ("add_u32", 2, test_primitives::add_u32),
        ("add_i32", 2, test_primitives::add_i32),
        ("echo_u8", 1, test_primitives::echo_u8),
        ("option_inc", 1, test_primitives::option_inc),
        ("result_to_int", 1, test_primitives::result_to_int),
        ("sum_list", 1, test_list::sum_list),
        ("make_list", 0, test_list::make_list),
        ("term_debug", 1, test_term::term_debug),
        ("term_eq", 2, test_term::term_eq),
        ("term_cmp", 2, test_term::term_cmp),
        ("sum_map_values", 1, test_map::sum_map_values),
        ("map_entries_sorted", 1, test_map::map_entries_sorted),
        ("map_from_arrays", 2, test_map::map_from_arrays),
        ("resource_make", 0, test_resource::resource_make),
        (
            "resource_set_integer_field",
            2,
            test_resource::resource_set_integer_field
        ),
        (
            "resource_get_integer_field",
            1,
            test_resource::resource_get_integer_field
        ),
        (
            "resource_make_immutable",
            1,
            test_resource::resource_make_immutable
        ),
        (
            "resource_immutable_count",
            0,
            test_resource::resource_immutable_count
        ),
        ("atom_to_string", 1, test_atom::atom_to_string),
        ("atom_equals_ok", 1, test_atom::atom_equals_ok),
        ("binary_to_atom", 1, test_atom::binary_to_atom),
        (
            "binary_to_existing_atom",
            1,
            test_atom::binary_to_existing_atom
        ),
        (
            "make_shorter_subbinary",
            1,
            test_binary::make_shorter_subbinary
        ),
        ("parse_integer", 1, test_binary::parse_integer),
        ("binary_new", 0, test_binary::binary_new),
        ("unowned_to_owned", 1, test_binary::unowned_to_owned),
        ("realloc_shrink", 0, test_binary::realloc_shrink),
        ("realloc_grow", 0, test_binary::realloc_grow),
        ("encode_string", 0, test_binary::encode_string),
        ("decode_iolist", 1, test_binary::decode_iolist),
        ("threaded_fac", 1, test_thread::threaded_fac),
        ("threaded_sleep", 1, test_thread::threaded_sleep),
        ("send_all", 2, test_env::send_all),
        ("sublists", 1, test_env::sublists),
        ("tuple_echo", 1, test_codegen::tuple_echo),
        ("record_echo", 1, test_codegen::record_echo),
        ("map_echo", 1, test_codegen::map_echo),
        ("struct_echo", 1, test_codegen::struct_echo),
        ("unit_enum_echo", 1, test_codegen::unit_enum_echo),
        ("untagged_enum_echo", 1, test_codegen::untagged_enum_echo),
        (
            "untagged_enum_with_truthy",
            1,
            test_codegen::untagged_enum_with_truthy
        ),
        ("newtype_echo", 1, test_codegen::newtype_echo),
        ("tuplestruct_echo", 1, test_codegen::tuplestruct_echo),
        ("newtype_record_echo", 1, test_codegen::newtype_record_echo),
        (
            "tuplestruct_record_echo",
            1,
            test_codegen::tuplestruct_record_echo
        ),
        ("dirty_cpu", 0, test_dirty::dirty_cpu, DirtyCpu),
        ("dirty_io", 0, test_dirty::dirty_io, DirtyIo),
        ("sum_range", 1, test_range::sum_range),
    ],
    Some(on_load)
);

fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    test_resource::on_load(env);
    test_atom::on_load(env);
    true
}
