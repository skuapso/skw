-module(skw_routes).
-behaviour (route_handler).
-export ([
  init/2,
  finish/2
  ]).

init(Config, State) ->
  RequestBridge = wf_context:request_bridge(),
  Path = RequestBridge:path(),
  Module = search_route_module(Config, list_to_binary(Path)),
  wf_context:page_module(Module),
  wf_context:path_info(Path),
  {ok, Path}.

finish(_Config, Path) ->
  {ok, Path}.

search_route_module([], _Path) ->
  skw;
search_route_module([{Prefix, Module} | T], Path) ->
  L = byte_size(Prefix),
  case Path of
    <<Prefix:L/binary, _Rest/binary>> -> Module;
    _               -> search_route_module(T, Path)
  end.
