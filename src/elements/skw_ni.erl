% vim: sw=4 ts=4 et ft=erlang

-module (skw_ni).
-include("elements.hrl").
-compile(export_all).

render_element(#skw_ni{msg = Module}) ->
    #panel{ body = ["not imlemented<br>", Module]}.
