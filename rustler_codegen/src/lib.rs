#![crate_type="dylib"]
#![cfg_attr(not(feature = "with-syntex"), feature(plugin, plugin_registrar, quote, rustc_private))]

extern crate syn;
//use syn::aster;
#[macro_use]
extern crate quote;

#[cfg(not(feature = "with-syntex"))]
extern crate syntax;
//extern crate rustc;
#[cfg(not(feature = "with-syntex"))]
extern crate rustc_plugin;

#[cfg(feature = "with-syntex")]
extern crate syntex;
#[cfg(feature = "with-syntex")]
extern crate syntex_syntax as syntax;
#[cfg(feature = "with-syntex")]
use std::path::Path;

mod util;
mod tuple;
mod map;

#[cfg(feature = "with-syntex")]
pub fn expand<S, D>(src: S, dst: D) -> Result<(), syntex::Error>
    where S: AsRef<Path>, D: AsRef<Path>
{
    let mut reg = syntex::Registry::new();

    reg.add_decorator("derive_MapTermTarget", map_transcoder_decorator_shim);
    reg.add_decorator("derive_TupleTermTarget", tuple_transcoder_decorator_shim);

    reg.expand("", src.as_ref(), dst.as_ref())
}

macro_rules! shim_syn_decorator {
    ($decorator_name:ident, $shim_name:ident, $wrapped:path) => {
        fn $shim_name(
            cx: &mut syntax::ext::base::ExtCtxt,
            span: syntax::codemap::Span,
            meta_item: &syntax::ast::MetaItem,
            annotatable: &syntax::ext::base::Annotatable,
            push: &mut FnMut(syntax::ext::base::Annotatable),
        ) {
            let item = match *annotatable {
                syntax::ext::base::Annotatable::Item(ref item) => item,
                _ => {
                    cx.span_err(
                        meta_item.span,
                        "decorated item must be either struct or enum"
                        );
                    return;
                },
            };

            let s = syntax::print::pprust::item_to_string(item);
            let syn_item = syn::parse_macro_input(&s).unwrap();

            let expanded = match $wrapped(&syn_item) {
                Ok(expanded) => expanded.to_string(),
                Err(msg) => {
                    cx.span_err(span, &msg);
                    return;
                }
            };

            use syntax::parse;
            let name = stringify!($decorator_name).to_string();
            let sess = parse::ParseSess::new();
            let impl_item = parse::parse_item_from_source_str(name, expanded, &sess);
            push(syntax::ext::base::Annotatable::Item(impl_item.unwrap().unwrap()));
        }
    };
}

shim_syn_decorator!(NifTuple, tuple_transcoder_decorator_shim, tuple::transcoder_decorator);
shim_syn_decorator!(NifMap, map_transcoder_decorator_shim, map::transcoder_decorator);

#[cfg(not(feature = "with-syntex"))]
#[plugin_registrar]
pub fn register(reg: &mut rustc_plugin::Registry) {
    reg.register_syntax_extension(
        syntax::parse::token::intern("derive_MapTermTarget"),
        syntax::ext::base::MultiDecorator(Box::new(map_transcoder_decorator_shim)));
    reg.register_syntax_extension(
        syntax::parse::token::intern("derive_TupleTermTarget"),
        syntax::ext::base::MultiDecorator(Box::new(tuple_transcoder_decorator_shim)));
}
