---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---
# OcamlFRP 

OCamlFRP is an OCaml library for reactive programming, which uses arrow operators, similar to those found in Yampa, to construct stream functions. Streams are defined in the Ocamlfrp.Streams module and have the type 'a stream.

## Streams 
Streams can be produced and consumed using the following functions:
{% highlight ocaml %}
  val coiterate : ('s -> 'a * 's) -> 's -> 'a stream
  val fold : 'a. ('a -> 'a) -> 'a -> 'a stream
  val constant : 'a. 'a -> 'a stream
  val consume : 'a stream -> ('a -> bool) -> float option -> unit
{% endhighlight %}

- [coiterate f s] constructs a stream using the coiterator [(f, s)], where [f] is the function and [s] is the initial state.
- [fold f x] generates a stream by iteratively applying the function [f] to the current value, starting with [x].
- [constant x] returns a constant stream where every element has the value [x].
- [consume s f t] applies the function [f] to all elements of the stream [s], with a delay of [t] between each application.

### Examples 
{% highlight ocaml %}
  let positives = fold ((+) 1) 0 
  let _ = consume positives (fun n -> print_int n; true)
{% endhighlight %}


## Stream Functions
Streams are not intended to be used directly. Instead, users should rely on stream functions to manipulate them.
Stream functions are defined in the Ocamlfrp.Arrows module.

{% highlight ocaml %}
type ('a,'b) sf 
val arr : ('a -> 'b) -> ('a, 'b) sf
val first : ('a, 'b) sf -> ('a * 'c, 'a * 'c) sf
val (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf
val (loop) : ('a * c, 'b * 'c) sf -> ('a, 'b) sf
{% endhighlight %}

## Environment 

The first step in running an SF program is to write an appropriate module of type Environment and instantiate the Engine functor with that module. The resulting instance provides a run function that can be used to execute the program.

{% highlight ocaml %}
  val run ('a,'b) sf -> float option -> unit
{% endhighlight %}

{% highlight ocaml %}
module type Environment = 
  sig 
    type appinput 
    type appoutput
    val init : unit -> unit
    val stop : unit -> unit 
    val input : bool -> appinput 
    val output : appoutput -> bool  
  end 
  {% endhighlight %}

### Example 

{% highlight ocaml %}
module Std : Environment with type appinput = string and type appoutput = string = 
  struct 
    type appinput = string 
    type appoutput = string 
    let init _ = ()
    let stop _ = ()
    let input _ = read_line ()
    let output s = print_endline s; true
  end 

module E = Engine(Std)

let _ = 
  let f (x,y) = dup (y ^ x) in
    E.run (loop (arr f) "") (Some 0.001)
{% endhighlight %}

  

