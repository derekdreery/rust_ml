# ML syntax for Rust (for fun)

Just for fun, and also to show off the power of rust macros, I'm going to experiment with an [ML]-like (specifically [Haskell]-like) syntax for Rust. I'm going to see how far I can push it before things go wrong. To save myself the effort of writing a tokenizer, I'm going to start by using a rust proc macro, which will give me a token stream to manipulate.

The first challenge is to parse our new syntax. I'm going to start with the following simple example:

```haskell
add1 :: u8 -> u8
add1 a = a + 1
```

To start with, I'm only going to make the parser work on 1 function at a time, otherwise I'd have to think about how to handle ambiguities, given that the rust tokenizer doesn't report whitespace. It would be nice to overcome this limitation, but I'm not sure it will be possible yet, at least in proc macros.

So, for my way of approaching writing proc macros and given that we are parsing something that isn't valid rust, the first step is to define our [abstract syntax tree (AST)][AST]. It's going to capture all parts of the syntax, including stuff that only has one value (like a `=` token), so that if we want to report errors later we can tie diagnostics to span any token. To do this, we basically write a recipe for a valid macro in terms of a sequence of objects that turns into a struct. Sometimes, there may be multiple valid sequences of tokens, and in that case we would use an enum and lookahead to decide which variant to parse. If you want to find out more about this stage (which can get a lot more technical if you want), search for ["formal grammar"] on the internet.

In our case, we expect a single sequence of tokens (with some simple looping for arguments) so I can use a single struct to represent the AST. My first attempt is below, and hopefully it won't be too far away from a correct one. I'll tag each bit of the struct with its corresponding source in the example above.

```rust,no_run,noplayground
struct MlFn {
    // add1
    ident: Ident,
    // ::
    doublec: Token![::],
    // u8 -> u8
    types: Punctuated<Type, Token![->]>,
    // add1
    ident2: Ident,
    // in
    args: Vec<Ident>,
    // =
    eq: Token![=],
    // in + 1
    body: Block,
}
```

For the moment, we're not doing any transformation on the body of the function: we are just parsing it as the inside of a Rust block, so the content of a function is just normal rust syntax for now.

Next job is to implement [`Parse`] for our AST. Our input to this stage is a stream of tokens (if you're working with source directly you would need to [lex] it first). Because we made our struct represent the code we expect, parsing almost becomes just a sequence of calls to [`parse`]:

```rust,no_run,noplayground
impl Parse for MlFn {
    fn parse(s: ParseStream) -> Result<Self> {
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
            body: s.parse()?,
        })
    }
}
```

There are 2 places where we can't just call `parse`. The first is `parse_separated_nonempty`, which tells `syn` to parse a sequence of the inner types (in this case `syn::Type`s), separated by the `->` token. The second is `args`, where we keep looking ahead for a `=` token, because we want to parse 0 or more idents followed by a `=`. The code above is ideomatic use of `syn`: all the types in `syn` use this pattern to define their parsers. To parse this we only ever have to lookahead a maximum of 1 times, meaning the parser will run fast (see [LR parsers]).

Now we can write our proc macro

```rust,no_run,noplayground
#[proc_macro]
pub fn rust_ml(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as MlFn);
    todo!()
}
```

and test it using a simple test (in `example`).

```rust,no_run,noplayground
use rust_ml::rust_ml;

rust_ml! {
    add1 :: u8 -> u8
    add1 a = a + 1
}

fn main() {
    assert_eq!(add1(3), 4);
}
```

Let's see how it goes...

```text
error: proc macro panicked
 --> src/main.rs:3:1
  |
3 | / rust_ml! {
4 | |     add1 :: u8 -> u8
5 | |     add1 a = a + 1
6 | | }
  | |_^
  |
  = help: message: not yet implemented
```

Woop woop: we failed, but only because we hit our `todo!()`, meaning that we parsed the contents! :)

Next up, let's actually output some code (using [`quote::quote`]):

```rust,no_run,noplayground
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
```

> Actually, when I was writing this, it took me a while to get it working. For a long time I couldn't understand the error message I was getting, but then I remembered [`cargo-expand`], which very quickly showed me that I was getting the ident and the type the wrong way round for the function arguments. Always a silly mistake with me!

Not too much more complicated! Ok, I cheated a bit, by assuming that we have some methods defined on `MlFn`. Let me post those below:

```rust,no_run,noplayground
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
```

This is where the bulk of the work happens. The `check` function's purpose is to check our code for any semantic errors, which in our case are things like the idents for the function definition and implementation not matching. We should also have exactly 1 more type argument than argument name, because there must be exactly 1 return type. We don't allow functions that don't return for now, but the user can simply add `-> ()` to the end if they don't want to return a value.

> We don't enforce purity or even immutability: this is ML syntax but not ML semantics.

You'll see that we construct a `syn::Error` object when we find some invariant that isn't upheld. In addition to the error message, this type contains a span that will be used by the compiler when reporting the error. In this way, we can produce understandable and useful diagnostics for users of our proc macro, hopefully making it as user-friendly as the compiler itself. This is why we kept all those pesky tokens in our AST.

Now let's see what happens when we run the code:

```text

```

Nothing. Meaning our assert passed! :)

The full code is available [on github][github.com/derekdreery/rust_ml]. Feel free to have a browse, with the caveat that trying to do anything useful with the code is not a good idea™.

[ML]: https://en.wikipedia.org/wiki/ML_(programming_language)
[Haskell]: https://www.haskell.org/
[AST]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
["formal grammar"]: https://en.wikipedia.org/wiki/Formal_grammar
[`cargo-expand`]: https://github.com/dtolnay/cargo-expand
[`Parse`]: https://docs.rs/syn/1.0.54/syn/parse/trait.Parse.html
[`parse`]: https://docs.rs/syn/1.0.54/syn/parse/struct.ParseBuffer.html#method.parse
[`quote::quote`]: https://docs.rs/quote/1.0.7/quote/index.html
[LR parsers]: https://en.wikipedia.org/wiki/LR_parser
[github.com/derekdreery/rust_ml]: https://github.com/derekdreery/rust_ml
