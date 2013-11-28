-module(action_context_menu).

-compile([export_all]).

-include("elements.hrl").

render_action(#context_menu{}) -> #event{type = mousedown}.
