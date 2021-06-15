```ocaml
# #require "preface"
```

# Using Freer to handle effects

Effects handling is always a hot topic. Although it is possible that
everything described in this guide will become obsolete as soon as
OCaml allows the description of "_user-defined effects_", as this is not
yet in the medium-term roadmap, Preface proposes a naive approach to
effects management (to be read as "a final encoding of the separation
between effects performance and effects interpretation).

The idea behind the handling of effects in Preface is to cover
"_user-defined effects_", i.e. the ability for the user of the library
to describe a set of effects that a program can propagate. This
program is in fact "just" a description of a program and it becomes
necessary to interpret it, i.e. to execute it by giving a concrete
meaning to each of the propagated effects.

In concrete terms, a programme is described **denotationally** where effects are
just descriptions, and the act of **interpreting** (or _handling_) these effects
amounts to giving them an **operational meaning**.

This separation offers a small cost of execution but introduces, in
our view, large benefits, for example:

- Effectful program becomes easier to unit test
- The logic of a program is no longer platform dependent, it is the
  interpreter that determines the execution platform
- The effects propagated by a program are reflected in the signature

On the other hand, it is not a panacea, as said above, it introduces
costs at runtime and the composition of the programs is not as elegant
as that proposed in the original Multicore-OCaml papers. Until this is
available, let's not deprive ourselves of `Freer` to describe pure
programs!

## The famous example of the teletype (or the "Hello World" of effectful programs)

Although the example may seem a little artificial, it captures the
essence of the separation between the description of a program and its
interpretation. Let's look at this software:

```ocaml non-deterministic=command
let () = print_endline "Hello, what's your name?"
let name = read_line ()
let () = print_endline ("Hello " ^ name ^ "! Enjoy this effectful program")
```

### Describing and interpreting operations using an ADT and a list

This program, although very simple, is very complicated to unit test,
we could try to transform it into a value and interpret it. First,
let's describe the operations of our program, here, the ability to
write to the standard output and read the standard input:

```ocaml
type operation =
  | Print of string
  | Read
```

And we could describe our program as a list of operations:

```ocaml
let program = [
  Print "Hello, what's your name?"
; Read
; Print "Hello, thanks for filling your name..."
; Print "But it's very sad... I don't know how to get it back..."
]
```

And to interpret it, you can simply use the `iter` function:

```ocaml
let run program =
  List.iter (function
    | Print message -> print_endline message
    | Read ->
       (* Normally, here we would use [read_line] but for
          the purposes of this guide, we will just ...
          do nothing. *)
       print_endline "I read something"
  ) program
```

And now we can run our program!

```ocaml
# run program ;;
Hello, what's your name?
I read something
Hello, thanks for filling your name...
But it's very sad... I don't know how to get it back...
- : unit = ()
```

Oh yes, it works! `program` is "pure" and only the `run` function is
impure. But, I left a clue in the program description... how to pass
the result of the `Read` line to the tail of the program? With such a
naive approach, this is unfortunately impossible, fortunately, Preface
offers a particular **monad** to describe programs and provide an
interpreter.

### Describing and interpreting operations using an ADT and `Freer`

As in the example that did not work, we will first describe the set of
operations that we want to be able to perform, I call the type that
will describe the `io` operations, because basically I want to do
input and output:

```ocaml
type _ io =
  | Print : string -> unit io
  | Read : string io
```

Unlike our first proposal (the one that didn't work) the type of our
operations is parameterised. This parameter corresponds to the
"**normal form**" of our program. Now we will derive, using [Freer
monad](https://ocaml-preface.github.io/preface/Preface_make/Freer_monad/Over/index.html)
all the necessary piping to describe pure programs! A `freer monad` is
a `monad` built on top of a type:

```ocaml non-deterministic=command
module IO = Preface.Make.Freer_monad.Over (struct
  type 'a t = 'a io
end)
```

We now need to lift our constructors (`Print` and `Read`) into
functions that will be of type `'a IO.t`, for this I can use the
`perform` function freshly derived from the construction of our Freer
Monad. For convenience, I will describe them directly in the `IO`
module.

```ocaml
module IO = struct
  include Preface.Make.Freer_monad.Over (struct
     type 'a t = 'a io
   end)

   let print message = perform (Print message)
   let read = perform Read
end
```

Now we have all the ingredients to rewrite our program! To be as close
as possible to my original description, I'll use `let operators`. And
yes, since the description of a program is of type `a IO.t` (which is
a monad) we have to use `bind` to sequence our
operations. Fortunately, operators allow us to get syntactically
closer to a direct style.

```ocaml
let program =
  let open IO.Monad.Syntax in
  let* () = IO.print "Hello, what's your name?" in
  let* name = IO.read in
  IO.print ("Hello " ^ name ^ "! Enjoy this program")
```

As you can see, the programme is very similar to the one we wrote at
the beginning of the guide. However, it does nothing!

```ocaml
# program ;;
- : unit IO.t = IO.Bind (Print "Hello, what's your name?", <fun>)
```

Although we don't care about the value of our program here, we can see
that its type is `unit IO.t`.

#### Let's interpret the program

Now that we have a program description, let's write the `run`
function, which previously was just applying the `iter` function to
our list of operations.

The `IO`
[module](https://ocaml-preface.github.io/preface/Preface_make/Freer_monad/Over/index.html)
exposes a `run` function which takes a
[`handler`](https://ocaml-preface.github.io/preface/Preface_make/Freer_monad/Over/index.html#type-handler)
and a program as arguments. This handler will interpret each of our
effects, propagated by our program, for example:

```ocaml
let run program =
  let handler : type b. (b -> 'a) -> b IO.f -> 'a =
    fun resume -> function
      | Print message ->
        let () = print_endline message in
        resume ()
      | Read ->
         (* As before, here I should call the [read_line] but
            for the purposes of the demonstration I will assume
            that [read_line] returns "Xavier" all the time. *)
         resume "Xavier"
    in
  IO.run { handler } program
```

Gee, what madness quantification, and yes, an `handler` takes as
argument a `continuation` and an effect to be
interpreted. Fortunately, it is generally always the same anotation
that should be written. For example, in the code of
[Wordpress](https://github.com/xhtmlboi/wordpress/blob/main/lib/generator.ml#L124)
(a static blog generator written using Preface), we can see that the
anotation is approximately the same, even though it has many more
effects than our example.

Let us look at the interpretation of our program:

```ocaml
# run program ;;
Hello, what's your name?
Hello Xavier! Enjoy this program
- : unit = ()
```

Superb, everything works as planned!

This way of separating the description of the programme from its
interpretation brings many benefits. From my point of view, the
biggest benefit is the ease of writing unit tests for effect
programs. For example, once again borrowing an example from WordPress
(damn... that name), a complete filesystem is mocked, only by providing
another interpreter/handler, so that you never have to query the real
filesystem.

## Main difference with first-class-modules

This approach to interpreting effects _posteriorly_ is a kind of
dependencies injection. It's hard not to wonder why we didn't just use
first-class modules, isn't it? For example, define our effects, as
dependencies, in a module type:

```ocaml
module type IO = sig
  val print : string -> unit
  val read : unit -> string
end
```

And now, let's define our program as "taking as argument" a module
respecting this signature:

```ocaml
let program_aux (module D : IO) =
  let () = D.print "Hello, what's your name?" in
  let name = D.read () in
  D.print ("Hello " ^ name ^ "! Enjoy this program")
```

The interpretation of the program seems more natural:

```ocaml
module IO_mod = struct
  let print = print_endline
  let read () =
     (* As before, here I should call the [read_line] but
        for the purposes of the demonstration I will assume
        that [read_line] returns "Xavier" all the time. *)
    "Xavier"
end
```

And as we can see, the result looks exactly the same:

```ocaml
# program_aux (module IO_mod) ;;
Hello, what's your name?
Hello Xavier! Enjoy this program
- : unit = ()
```

So why bother handling complicated objects that generate complex
quantifications? Well, because the description of the effects using a
_Freer Monad_ is more "powerful". The execution of the `run` function
highlights the fact that the handler function takes two arguments:

- in second place, the effect to be interpreted
- in first position: the **captured continuation**

This continuation allows the handler to decide whether to **continue
the execution of the program or not**. This makes it possible to
define handlers capable of simulating specific behaviour, often
encoded as special cases of the language. For example the `try/catch`
exception construct found in many languages.

In addition to the ability to let the handler decide whether or not it
wants to continue execution, as a program described using _Freer_ is a
monad, it is possible to take advantage of all the benefits of its
monadic behaviour, e.g. traversing lists of program effects to reduce
them to a single list effect (for example).

In this section, higher-order modules were presented as a restricted form of
effect handler, where the handler always resumes the programme. This observation
also applies to **Functors** (parameterised modules), of course.

## Conclusion

_Freer monad_ offers an alternative to first class modules for
dependency injection, while adding the ability to continue, or not,
the execution of the program. Even if it doesn't allow to apply an
equational reasoning like the "real algebraic effects" described in
the various publications related to the development of "Multicore
OCaml", their API allows to get close to it. And the use of `let operators` makes it possible to write programs in a style close to the
direct style.

Controlling the continuation of the programme offers a lot of flexibility,
however, it can cause problems when using linear resources (a file handler for
example). A linear resource should always be passed to the handler so that not
resuming the continuation allows the resource to be properly closed, and passed
to the continuation to avoid memory leaks.
