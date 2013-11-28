-module(skw_object).

-compile([export_all]).

-include("elements.hrl").

-define(is_element(Item), 
    is_tuple(Item) and (
    (element(1, Item) =:= object) or
    (element(1, Item) =:= object_model)
)).

render_element(#object{}) ->
  [].

json2rec(#object{} = Item) -> Item;
json2rec(#object_model{} = Item) -> Item;
json2rec(JSON) ->
  case type(JSON) of
    Type when ?is_element(Type) -> json2rec(Type, JSON);
    _ -> skw:to_rec(JSON)
  end.

json2rec(Object, [{_Key, <<"">>} | T]) ->
  json2rec(Object, T);
json2rec(#object{} = Object, [{<<"type">>, Type} | T]) ->
  Type = <<"object">>,
  json2rec(Object, T);
json2rec(#object{} = Object, [{<<"no">>, Value} | T]) ->
  json2rec(Object#object{no = Value}, T);
json2rec(#object{} = Object,  [{<<"object">>, Value} | T]) ->
  json2rec(Object#object{id = skw:to_integer(Value)}, T);
json2rec(#object{} = Object, [{<<"specialization_id">>, Value} | T]) ->
  json2rec(Object#object{specialization_id = Value}, T);
json2rec(#object{} = Object, [{<<"model_id">>, Value} | T]) ->
  json2rec(Object#object{model_id = Value}, T);
json2rec(#object{} = Object, [{<<"terminal_id">>, Value} | T]) ->
  json2rec(Object#object{terminal_id = Value}, T);

json2rec(#object_model{} = Model, [{<<"type">>, Type} | T]) ->
  Type = <<"model">>,
  json2rec(Model, T);
json2rec(#object_model{} = Model, [{<<"title">>, Value} | T]) ->
  json2rec(Model#object_model{title = Value}, T);

json2rec(Item, [{_Key, _Val} | T]) ->
  json2rec(Item, T);
json2rec(Item, []) -> Item.


dropped(#object{} = Object, #group{id = GId} = Group) ->
  set(Object, <<"group_id">>, GId),
  wf:send_global(ui_event, {move, Object, Group});
dropped(#object{} = Object, [{_,_} | _] = DroppedOn) ->
  dropped(Object, skw:to_rec(DroppedOn));
dropped(Item, DroppedOn) ->
  lager:warning("dropping ~p on ~p not implemented", [Item, DroppedOn]).

set(#object{} = Item, Field, [{<<"new">>, {struct, Req}}]) ->
  ReqItem = json2rec(Req),
  lager:debug("new ~p", [ReqItem]),
  {ok, Tag} = skw:delayed_event(?MODULE, set, fun(X) -> [Item, Field, X] end),
  skw:new(Tag, ReqItem);
set(#object{id = Id} = Item, Field, Val) ->
  wf:q({sql, execute, {
        skw:join([<<"update objects.data set ">>, Field, <<"=$1 where id=$2">>], ""),
        [Val, Id]}}),
  wf:send_global(ui_event, {set, Item, [{Field, Val}]}).

proplist(#object{
        id = Id,
        no = No,
        specialization_id = Spec,
        model_id = Model,
        terminal_id = Terminal}) ->
  skw:remove_undefined([
      {type, object},
      {object, Id},
      {no, No},
      {specialization_id, Spec},
      {model_id, Model},
      {terminal_id, Terminal}]);
proplist(#object_model{
        id = Id,
        title = Title}) ->
  skw:remove_undefined([
      {type, object_model},
      {object_model, Id},
      {title, Title}]).

title(#object {no = No}) -> No.

type(JSON) -> base_type(proplists:get_value(<<"type">>, JSON)).
base_type(<<"object">>) -> #object{};
base_type(<<"object_model">>) -> #object_model{};
base_type(<<"model">>) -> #object_model{};
base_type(Item) -> skw:base_type(Item).

new(Tag, #object_model{title = undefined} = Model) ->
  skw:js(<<"Skuapso.object.model.new">>, [Tag, proplist(Model)]);
new(_Tag, #object_model{title = Title}) ->
  Reply = {ok, Id} = wf:q({sql, insert, {objects, models, [{title, Title}]}}),
  wf:send_global(ui_event, {new, {object, model, Id, Title}}),
  Reply.
