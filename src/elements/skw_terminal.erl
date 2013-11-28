-module(skw_terminal).

-compile(export_all).

-include("elements.hrl").

-define(is_element(Item), 
    is_tuple(Item) and (
    (element(1, Item) =:= terminal) or
    (element(1, Item) =:= terminal_model)
)).

json2rec(#terminal{} = Item) -> Item;
json2rec(#terminal_model{} = Item) -> Item;
json2rec(JSON) ->
  case type(JSON) of
    Type when ?is_element(Type) -> json2rec(Type, JSON);
    _ -> skw:to_rec(JSON)
  end.

json2rec(#terminal{} = Terminal, [{<<"type">>, Type} | T]) ->
  Type = <<"terminal">>,
  json2rec(Terminal, T);
json2rec(#terminal{} = Terminal, [{<<"id">>, Id} | T]) ->
  json2rec(Terminal#terminal{id = skw:to_integer(Id)}, T);
json2rec(#terminal{} = Terminal, [{<<"title">>, Title} | T]) ->
  json2rec(Terminal#terminal{uin = skw:to_integer(Title)}, T);
json2rec(#terminal{} = Terminal, [{<<"model_id">>, Model} | T]) ->
  json2rec(Terminal#terminal{model_id = skw:to_integer(Model)}, T);
json2rec(#terminal{} = Terminal, [{<<"uin">>, UIN} | T]) ->
  json2rec(Terminal#terminal{uin = skw:to_integer(UIN)}, T);
json2rec(#terminal{} = Terminal, [{<<"serial_no">>, SerialNo} | T]) ->
  json2rec(Terminal#terminal{serial_no = SerialNo}, T);

json2rec(Item, [{_Key, _Val} | T]) ->
  json2rec(Item, T);
json2rec(Item, []) -> Item.

type(JSON) -> base_type(proplists:get_value(<<"type">>, JSON)).
base_type(<<"terminal">>) -> #terminal{};
base_type(<<"terminal_model">>) -> #terminal_model{};
base_type(<<"model">>) -> #terminal_model{};
base_type(Item) -> skw:base_type(Item).

proplist(#terminal{
        id = Id,
        uin = UIN,
        serial_no = SerialNo,
        model_id = ModelId}) ->
  skw:remove_undefined([
      {id, Id},
      {uin, UIN},
      {serial_no, SerialNo},
      {model_id, ModelId}]).

new(Tag, #terminal{
        id = undefined,
        uin = UIN,
        serial_no = SerialNo,
        model_id = ModelId} = Item)
    when
    UIN =:= undefined;
    SerialNo =:= undefined;
    ModelId =:= undefined
    ->
  lager:debug("terminal is ~p", [Item]),
  skw:js("Skuapso.terminal.new", [
      skw:json_encode([{tag, wf:pickle(Tag)} | proplist(Item)])]),
  {ok, undefined};
new(_Tag, #terminal{
        id = undefined,
        uin = UIN,
        serial_no = SerialNo,
        model_id = ModelId}) ->
  Reply = {ok, Id} = wf:q({sql, insert, {terminals, data, [
          {uin, UIN},
          {serial_no, SerialNo},
          {model_id, ModelId}]}}),
  wf:send_global(ui_event, {new, {object, terminal, Id, UIN}}),
  Reply.
