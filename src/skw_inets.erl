-module(skw_inets).
-export ([do/1]).

-define(DEFAULT_ROUTES, [
    % Static directories
    {<<"/nitrogen">>, static_file},
    {<<"/js">>, static_file},
    {<<"/css">>, static_file},
    {<<"/html">>, static_file},
    {<<"/img">>, static_file}
    ]).

do(Info) ->
  RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
  ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
  nitrogen:init_request(RequestBridge, ResponseBridge),
  wf_handler:set_handler(skw_query, []),
  wf_handler:set_handler(skw_routes, ?DEFAULT_ROUTES),
  wf:switch_to_comet(),
  nitrogen:run().
