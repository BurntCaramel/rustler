use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::token::Comma;

pub fn transcoder_decorator(args: &syn::AttributeArgs, fun: &syn::ItemFn) -> TokenStream {
    let sig = &fun.sig;
    let name = &sig.ident;
    let inputs = &sig.inputs;
    let flags = schedule_flag(&args);
    let function = fun.into_token_stream();
    let arity = arity(inputs);
    let decoded_terms = extract_inputs(inputs);
    let argument_names = create_function_params(inputs);
    let erl_func_name = extract_attr_value(&args, "name")
        .map(|ref n| syn::Ident::new(n, Span::call_site()))
        .unwrap_or_else(|| name.clone());

    quote! {
        pub mod #name {
            pub const NAME: *const u8 = concat!(stringify!(#erl_func_name), "\0").as_ptr() as *const u8;
            pub const ARITY: u32 = #arity;
            pub const FLAGS: u32 = #flags as u32;
            pub const FUNC: unsafe extern "C" fn(
                nif_env: rustler::codegen_runtime::NIF_ENV,
                argc: rustler::codegen_runtime::c_int,
                argv: *const rustler::codegen_runtime::NIF_TERM
            ) -> rustler::codegen_runtime::NIF_TERM = {
                unsafe extern "C" fn nif_func(
                    nif_env: rustler::codegen_runtime::NIF_ENV,
                    argc: rustler::codegen_runtime::c_int,
                    argv: *const rustler::codegen_runtime::NIF_TERM
                ) -> rustler::codegen_runtime::NIF_TERM {
                    let lifetime = ();
                    let env = rustler::Env::new(&lifetime, nif_env);

                    let terms = std::slice::from_raw_parts(argv, argc as usize)
                        .iter()
                        .map(|term| rustler::Term::new(env, *term))
                        .collect::<Vec<rustler::Term>>();

                    fn wrapper<'a>(
                        env: rustler::Env<'a>,
                        args: &[rustler::Term<'a>]
                    ) -> rustler::codegen_runtime::NifReturned {
                        let result = std::panic::catch_unwind(move || {
                            #decoded_terms
                            #function
                            #name(#argument_names)
                        });

                        unsafe {
                            match result {
                                Ok(res) => {
                                 rustler::codegen_runtime::NifReturnable::as_returned(res, env)
                                }
                                Err(_err) => {
                                 let error: rustler::Error = rustler::Error::Atom("nif_panic");
                                 rustler::codegen_runtime::NifReturnable::as_returned(error, env)
                                }
                            }
                        }
                    }
                    wrapper(env, &terms).apply(env)
                }
                nif_func
            };
            pub const DEF_NIF_FUNC: rustler::codegen_runtime::DEF_NIF_FUNC = rustler::codegen_runtime::DEF_NIF_FUNC {
                arity: ARITY,
                flags: FLAGS,
                function: FUNC,
                name: NAME
            };
        }
    }
}

fn schedule_flag(args: &syn::AttributeArgs) -> TokenStream {
    let mut tokens = TokenStream::new();

    let valid = ["DirtyCpu", "DirtyIo", "Normal"];

    let flag = match extract_attr_value(&args, "schedule") {
        Some(value) => {
            if valid.contains(&value.as_str()) {
                syn::Ident::new(value.as_str(), Span::call_site())
            } else {
                panic!("Invalid schedule option `{}`", value);
            }
        }
        None => syn::Ident::new("Normal", Span::call_site()),
    };

    tokens.extend(quote! { rustler::SchedulerFlags::#flag });
    tokens
}

fn extract_attr_value<'a>(args: &'a syn::AttributeArgs, name: &str) -> Option<String> {
    use syn::{Lit, Meta, MetaNameValue, NestedMeta};

    for arg in args.iter() {
        if let NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) = arg {
            if path.is_ident(name) {
                if let Lit::Str(lit) = lit {
                    return Some(lit.value());
                }
            }
        }
    }

    None
}

fn extract_inputs<'a>(inputs: &'a Punctuated<syn::FnArg, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();

    for (i, item) in inputs.iter().enumerate() {
        let (name, typ) = if let syn::FnArg::Typed(ref typed) = item {
            (&typed.pat, &typed.ty)
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };

        let error = format!(
            "unsupported function argument type `{}` for `{}`",
            quote!(#typ),
            quote!(#name)
        );

        let arg = quote! {
            let #name: #typ = args[#i]
                .decode()
                .map_err(|_| #error)
                .expect(#error);
        };

        tokens.extend(arg);
    }

    tokens
}

fn create_function_params<'a>(inputs: &'a Punctuated<syn::FnArg, Comma>) -> TokenStream {
    let mut tokens = TokenStream::new();

    for item in inputs.iter() {
        let name = if let syn::FnArg::Typed(ref typed) = item {
            &typed.pat
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };

        tokens.extend(quote!(#name,));
    }

    tokens
}

fn arity<'a>(inputs: &'a Punctuated<syn::FnArg, Comma>) -> u32 {
    let arity: u32 = 0;

    for item in inputs.iter() {
        if let syn::FnArg::Typed(ref typed) = item {
            &typed.pat
        } else {
            panic!("unsupported input given: {:?}", stringify!(&item));
        };
    }

    arity
}
