%% -*- mode: nitrogen -*-
-module (skw_blank).
-compile(export_all).
-include("elements.hrl").

reflect() -> record_info(fields, skw_blank).

render_element(_Record = #skw_blank{}) -> <<>>.
