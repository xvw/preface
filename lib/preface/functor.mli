(** Modules for building {!Preface_specs.FUNCTOR} modules. *)

(** {1 Tutorial}

    In order to be modular, [Preface] offers multiple way to build a 
    {!Preface_specs.FUNCTOR}. In many case, you just have to use the 
    parametrized module {!Make_via_map}, but in some particular cases,
    you want to be able to write each component of a [Functor].


    {2 Basics}

    The most common way to build a [Functor] is to use the module 
    {!Make_via_map}. For example, here is the way to have a [Functor] 
    module inside an [Option] module:

    {[
      (* In: option.ml *)
      module Functor = Preface.Functor.Make_via_map(struct
          type 'a t = 'a option
          let map f = function 
            | Some x -> Some (f x)
            | None -> None 
        end)
    ]}

    {[
      (* In: option.mli *)
      module Functor : Preface_specs.FUNCTOR with type 'a t = 'a option
    ]}

    Now, your [Option] module is handling a [Functor] module and you'll be 
    able to use features offered by a [Functor]. For example:

    {[
      let result = 
        let open Option.Functor.Infix in 
        Some 10 
        <$> (fun x -> x + 12) 
        <$> (fun x -> x * 3)
        <$> (fun x -> x + 15)
    ]}

    {[val result : int option = Some 81]}

    {2 Advanced}

    In the previous example, we used an approach that uses a minimal
    interface to build all the functionality of a [functor]. Now, let's
    imagine that we want to provide our own implementation for [replace]. 
    To acheive that (an avoiding the combinatorial explosion, exposing a 
    lot of parametrized modules) we can use the module {!Make}. For example, 
    reimplement our [Option.Functor] in an another way:

    First, let's define our [Core] module.
    {[
      module Core : Preface_specs.Functor.CORE = struct
        type 'a t = 'a option
        let map f = function 
          | Some x -> Some (f x)
          | None -> None 
      end
    ]}

    Now, we can give a custom implementation of [Operation]. 

    {[
      module Operation : Preface_specs.Functor.OPERATION = struct
        type 'a t = 'a option 
        let replace value = function  
          | Some x -> Some value
          | None -> None
                        
        let void x = replace () x
      end
    ]}

    Since [Infix] are just infix shortcuts, We do not want to 
    have to write the module by hand, so we can use {!Make_infix}:

    {[ module Infix = Preface.Functor.Make_infix (Core) (Operation) ]}

    And now, we can just instanciate the [Functor]:

    {[ module Functor = Preface.Functor.Make (Core) (Operation) (Infix) ]}

    And in the [mli], same of the previous tutorial:
    {[module Functor : Preface_specs.FUNCTOR with type 'a t = 'a option]}

    This approach allow us to define multiple way to instanciate a [Functor]
    without producing a lot of paramtrized modules.

    {2 Conclusion}

    [Preface] makes it possible to construct functors in several different 
    ways. However, in many cases, the simple approach is recommended. 
    Use manual construction only if it is really necessary.
*)

(** {1 Documentation} *)

(** {2 Construction} 

    Standard way to build a [Functor].
*)

module Make_via_map (Core : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] with standard requirements ([map]).*)

(** {2 Manual construction} 
    
    Advanced way to build a [Functor], constructing and assembling a 
    component-by-component functor. (In order to provide your own 
    implementation for some features.)
*)

module Make
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t
(** Incarnation of a [Functor] using each components of 
    a [Functor].
*)

module Make_operation (Core : Preface_specs.Functor.CORE) :
  Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Operation] with standard Requirements ([map]). 
*)

module Make_infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Functor.INFIX with type 'a t = 'a Core.t
(** Incarnation of a [Functor.Infix] with functional API of a [Functor]. *)
