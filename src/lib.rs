extern crate proc_macro;

use itertools::izip;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned};
use syn::{
    parse::{self, Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Block, Error, FnArg, Ident, Stmt, Token, Type,
};

#[proc_macro]
pub fn rust_ml(input: TokenStream) -> TokenStream {
    // parsing
    let input = parse_macro_input!(input as MlFn);

    if let Err(e) = input.check() {
        return e.to_compile_error().into();
    }

    //panic!("{:#?}", input);

    // codegen
    let ident = &input.ident;

    let args = input.args();
    let ret = input.ret();
    let body = &input.body;

    quote!(
        pub fn #ident(#(#args),*) -> #ret {
            #(#body)*
        }
    )
    .into()
}

#[derive(Debug)]
struct MlFn {
    ident: Ident,
    doublec: Token![::],
    types: Punctuated<Type, Token![->]>,
    ident2: Ident,
    args: Vec<Ident>,
    eq: Token![=],
    body: Vec<Stmt>,
}

impl MlFn {
    /// Return an error if the input to the macro was invalid.
    ///
    /// TODO collect all the errors together and report them in one go.
    fn check(&self) -> Result<(), Error> {
        if self.ident != self.ident2 {
            return Err(Error::new(
                self.ident2.span(),
                "name of function in implementation doesn't match the name in declaration",
            ));
        }

        if self.types.len() != self.args.len() + 1 {
            return Err(Error::new(
                self.types.span(),
                "the number of types should be 1 more than the number of args",
            ));
        }
        Ok(())
    }

    /// The arguments as a sequence of token streams.
    fn args<'a>(&'a self) -> impl Iterator<Item = TokenStream2> + 'a {
        // we already checked the lengths are valid
        let mut tys: Vec<_> = self.types.iter().collect();
        tys.pop(); // lose the return type.
        izip!(tys, &self.args).map(|(ty, ident)| quote!(#ident: #ty))
    }

    /// The return value as a type.
    fn ret(&self) -> Type {
        // we already checked the lengths are valid
        self.types.iter().last().unwrap().clone()
    }
}

impl Parse for MlFn {
    fn parse(s: ParseStream) -> parse::Result<Self> {
        Ok(MlFn {
            ident: s.parse()?,
            doublec: s.parse()?,
            types: Punctuated::parse_separated_nonempty(s)?,
            ident2: s.parse()?,
            args: {
                let mut args = vec![];
                while !s.peek(Token![=]) {
                    args.push(s.parse()?);
                }
                args
            },
            eq: s.parse()?,
            body: Block::parse_within(s)?,
        })
    }
}
