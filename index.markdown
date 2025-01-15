---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---

Ocaml FRP is an ocaml library for reactive programming. It relies on arrow operators as can be found in Yampa to construct stream functions.

Streams are defined in the module Ocamlfrp.Streams and have type 'a stream.

Stream can be produced and consumed using the following functions 
{% highlight ocaml %}
  val coiterate : ('s -> 'a * 's) -> 's -> 'a stream
  val fold : 'a. ('a -> 'a) -> 'a -> 'a stream
  val constant : 'a. 'a -> 'a stream
  val consume : 'a stream -> ('a -> bool) -> float option -> unit
{% endhighlight %}

- [coiterate f s] constructs a stream using the coiterator [(f, s)], 
  where [f] is the function and [s] is the initial state.
- [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x].
- [constant x] returns a constant stream where every element has the value x.
- [consume s f t] : applies the function f to all elements of the stream s, with a delay of t between two applications


Stream functions are defined in the module Ocamlfrp.Arrows 

{% highlight ocaml %}
val arr : ('a -> 'b) -> ('a, 'b) sf
val first : ('a, 'b) sf -> ('a * 'c, 'a * 'c) sf
val (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf
val (loop) : ('a * c, 'b * 'c) sf -> ('a, 'b) sf
{% endhighlight %}
