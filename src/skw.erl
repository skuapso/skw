-module(skw).

-compile([export_all]).

-include("elements.hrl").

main() ->
  wf:comet_global(fun() -> ui_events_loop() end, ui_event),
  wf:comet(fun() -> delayed_events_loop() end, delayed_event),
  #template{file = <<"www/templates/index.html">>}.

models() ->
  Models = lists:map(fun(X) ->
        Id = proplists:get_value(id, X),
        Title = proplists:get_value(title, X),
        {Id, Title}
    end, wf:q({sql, select, {ui, models, []}})),
  join(["Skuapso.object.model = ", json_encode(Models)], "").

terminals() ->
  Terminals = lists:map(fun(X) ->
          Id = proplists:get_value(id, X),
          Title = proplists:get_value(title, X),
          {Id, Title}
      end, wf:q({sql, select, {ui, terminals, []}})),
  join(["Skuapso.object.terminal = ", json_encode(Terminals)], "").

terminal_models() ->
  Models = lists:map(fun(X) ->
          Id = proplists:get_value(id, X),
          Title = proplists:get_value(title, X),
          {Id, Title}
      end, wf:q({sql, select, {terminals, models, []}})),
  join(["Skuapso.terminal.model = ", json_encode(Models)], "").

event(Event) ->
  DataJSON = wf:q(data),
  Data = json_decode(DataJSON),
  lager:debug("data is ~p", [Data]),
  erlang:apply(?MODULE, event, [Event | Data]).

event(cancel, Tag) ->
  cancel(wf:depickle(Tag)).

event(new, Tag, Item) ->
  call_tagged_event(Item, new, Tag, []);
event(context_menu, <<"edit_", What/binary>>, Item) ->
  js("Skuapso.event.edit", [
      json_encode(Item),
      join(["'", What, "'"], "")
      ]);
event(context_menu, Action, Item) -> call_event(Item, Action);
event(drop, Item, DroppedOn) -> call_event(Item, dropped, [DroppedOn]).

event(set, Field, Value, Item) -> call_event(Item, set, [Field, Value]).

serial_data() ->
  Events = [
      context_menu,
      drop,
      set,
      new,
      cancel
      ],
  lists:map(fun(X) ->
        wf:f("Skuapso.serialized.~s = '~s';\n", [
            X, 
            wf_event:serialize_event_context(X, undefined, undefined, undefined, ?MODULE)
            ])
    end, Events).

id(Id) -> join(tuple_to_list(Id), <<"-">>).
class({Class, _}) -> class(Class);
class(Class) -> join([Class], <<"-">>).
classes(Classes) -> [class(Class) || Class <- Classes].

to_atom(A) when is_binary(A) -> binary_to_atom(A, latin1);
to_atom(A) when is_list(A)   -> list_to_atom(A);
to_atom(A) when is_atom(A)   -> A.

to_integer(undefined)             -> 0;
to_integer(I) when is_binary(I)   -> list_to_integer((binary_to_list(I)));
to_integer(I) when is_list(I)     -> list_to_integer(I);
to_integer(I) when is_integer(I)  -> I.
to_integer({Key, Val}, L) ->
  case lists:member(Key, L) of
    true -> {Key, to_integer(Val)};
    false -> {Key, Val}
  end.

to_list(S) when is_binary(S)      -> unicode:characters_to_list(S);
to_list(S) when is_list(S)        -> S.

to_binary(T) when is_tuple(T)     ->
  iolist_to_binary(lists:map(fun(X) -> to_binary(X) end, tuple_to_list(T)));
to_binary(I) when is_integer(I)   -> to_binary(integer_to_list(I));
to_binary(undefined)              -> <<>>;
to_binary(null)                   -> <<>>;
to_binary(S) when is_atom(S)      -> atom_to_binary(S, latin1);
to_binary([])                     -> <<>>;
to_binary(S) when is_list(S)      -> unicode:characters_to_binary(S);
to_binary(S) when is_binary(S)    -> S;
to_binary(S) -> wf:to_binary(S).

html_encode(Str, full_html) -> Str;
html_encode(Str, full) -> Str;
html_encode(Str, plain) -> iolist_to_binary(html_encode(Str));
html_encode(Str, _EncType) -> Str.

html_encode([]) -> [];
html_encode(<<>>) -> <<>>;
html_encode([H|T]) ->
  [html_encode_symbol(H) | html_encode(T)];
html_encode(<<H, T/binary>>) ->
  E = html_encode_symbol(H),
  [E | html_encode(T)].

html_encode_symbol(H) ->
  case H of
    $< -> <<"&lt;">>;
    $> -> <<"&gt;">>;
    $" -> <<"&quot;">>;
    $' -> <<"&#39;">>;
    $& -> <<"&amp;">>;
    $\n -> <<"<br>">>;
    _ -> list_to_binary([H])
  end.

binary_join(BinList, Separator) -> binary_join(<<>>, BinList, Separator).
binary_join(Joined, [], _Separator) -> Joined;
binary_join(<<>>, [H | T], Separator) ->
  binary_join(H, T, Separator);
binary_join(Joined, [H | T], Separator) ->
  binary_join(<<Joined/binary, Separator/binary, H/binary>>, T, Separator).

join(List, Separator) ->
  binary_join(lists:map(fun(X) -> to_binary(X) end, List), to_binary(Separator)).

json_decode(DataJSON) ->
  DataBin = nitro_mochijson2:decode(DataJSON),
  json_decode_params(DataBin).

json_decode_params(Data) when is_list(Data) ->
  lists:map(fun json_decode_param/1, Data);
json_decode_params(Data) ->
  json_decode_param(Data).

json_decode_param({struct, Item}) ->
  Item;
json_decode_param(Item) ->
  Item.

json_encode(Data) ->
  json_encode_param(Data).

json_encode_param(Item) when is_tuple(Item) ->
  ItemModule = element(3, Item),
  json_encode_param(ItemModule:proplist(Item));
json_encode_param(Item) ->
  ui:erl2json(Item).

to_rec([{_, _} | _] = Item) ->
  TypeBin = proplists:get_value(<<"type">>, Item),
  Type = to_atom(join([<<"skw_">>, TypeBin], "")),
  Type:json2rec(Item);
to_rec(JSON) ->
  to_rec(json_decode(JSON)).

title(Item) ->
  ItemModule = element(3, Item),
  ItemModule:title(Item).

call_event(Item, Action) -> call_event(Item, Action, []).
call_event(Item, Action, Args) when is_tuple(Item), is_atom(Action) ->
  ItemModule = element(3, Item),
  erlang:apply(ItemModule, Action, [Item | Args]);
call_event(Item, Action, Args) ->
  call_event(to_rec(Item), to_atom(Action), Args).

call_tagged_event(Item, new, Tag, [])
    when
    is_tuple(Item),
    not is_binary(Tag)
    ->
  new(Tag, Item);
call_tagged_event(Item, Action, Tag, Args)
    when
    is_tuple(Item),
    is_atom(Action),
    not is_binary(Tag) ->
  ItemModule = element(3, Item),
  erlang:apply(ItemModule, Action, [Tag, Item | Args]);
call_tagged_event(Item, Action, Tag, Args)
    when
    not is_tuple(Item);
    not is_atom(Action)
    ->
  call_tagged_event(to_rec(Item), to_atom(Action), Tag, Args);
call_tagged_event(Item, Action, Tag, Args)
    when
    is_binary(Tag)
    ->
  call_tagged_event(Item, Action, wf:depickle(Tag), Args).

ui_events_loop() ->
  receive
    {move, Item, To} ->
      js("Skuapso.event.move", [json_encode(Item), json_encode(To)]);
    {set, Item, Params} ->
      js("Skuapso.event.set", [json_encode(Item), json_encode(Params)]);
    {new, {Parent, Object, Id, Value}} ->
      lager:debug("new ~p.~p = {~p: ~p}", [Parent, Object, Id, Value]),
      wf:wire(join([<<"Skuapso.">>, Parent, <<".">>, Object,
                    <<"[">>, Id, <<"]='">>, Value, <<"';">>], ""));
    {Act, _Pid} when Act =:= 'JOIN'; Act =:= 'LEAVE' -> ok;
    'INIT' -> ok;
    Msg -> lager:warning("unhandled msg ~p", [Msg])
  end,
  ui_events_loop().

delayed_events_loop() -> delayed_events_loop([]).
delayed_events_loop(Events) ->
  NewEvents = receive
    {delayed_event, Pid, Module, Fun, ArgsFun} ->
      Tag = now(),
      lager:debug("new delayed event ~p", [Tag]),
      Pid ! {tag, Tag},
      [{Tag, {Module, Fun, ArgsFun}} | Events];
    {execute, Tag, Value} ->
      lager:debug("search for ~p to set value ~p", [Tag, Value]),
      case proplists:get_value(Tag, Events) of
        {Module, Fun, ArgsFun} ->
          erlang:apply(Module, Fun, ArgsFun(Value));
        Else ->
          lager:warning("not found delayed event for ~p: ~p", [{Tag, Value}, Else])
      end,
      proplists:delete(Tag, Events);
    {cancel, Tag} ->
      lager:debug('cancel ~p', [Tag]),
      proplists:delete(Tag, Events);
    {Act, _Pid} when Act =:= 'JOIN'; Act =:= 'LEAVE' -> Events;
    'INIT' -> lager:debug("init"), Events;
    Msg -> lager:warning("unhandled msg ~p", [Msg]), Events
  end,
  delayed_events_loop(NewEvents).

delayed_event(Module, Fun, ArgsFun) ->
  wf:send(delayed_event, {delayed_event, self(), Module, Fun, ArgsFun}),
  receive
    {tag, Tag} -> {ok, Tag};
    Reason -> {error, Reason}
  after 5000 -> {error, timeout}
  end.

executed(_Tag, undefined) -> ok;
executed(Tag, Value) -> wf:send(delayed_event, {execute, Tag, Value}).

cancel(Tag) -> wf:send(delayed_event, {cancel, Tag}).

js(Fun, Params) ->
  ParamsStr = join(Params, ","),
  Str = join([Fun, "(", ParamsStr, ");"], ""),
  wf:wire(Str),
  wf:flush().

js_encode(Str) -> js_encode(Str, <<>>).
js_encode(<<>>, Str) -> Str;
js_encode(<<$", Rest/binary>>, Encoded) ->
  js_encode(Rest, <<Encoded/binary, "\", $">>);
js_encode(<<A, Rest/binary>>, Encoded) ->
  js_encode(Rest, <<Encoded/binary, A>>).

base_type(<<"object">>) -> #object{};
base_type(<<"object_model">>) -> #object_model{};
base_type(<<"terminal">>) -> #terminal{}.

new(Tag, Item) when is_tuple(Item) ->
  ItemModule = element(3, Item),
  {ok, Reply} = ItemModule:new(Tag, Item),
  executed(Tag, Reply).

remove_undefined(L) ->
  lager:debug("removing undefined from ~p", [L]),
  L1 = [{A, B} || {A, B} <- L, B =/= undefined],
  [{A, B} || {A, B} <- L1, B =/= null].
