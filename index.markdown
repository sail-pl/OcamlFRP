---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---

Ocaml FRP is an ocaml library for reactive programming.




{% highlight ocaml %}
val arr : ('a -> 'b) -> ('a, 'b) sf
val first : ('a, 'b) sf -> ('a * 'c, 'a * 'c) sf
val (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf
val (loop) : ('a * c, 'b * 'c) sf -> ('a, 'b) sf
{% endhighlight %}
