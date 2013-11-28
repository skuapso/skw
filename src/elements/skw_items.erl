-module(skw_items).

-include("elements.hrl").
-compile([export_all]).

item({Type, Id}) -> Type:item(Id).

list() ->
  SQL = wf:q({sql, select, {ui, items_tree, []}}),
  List = lists:map(fun(X) ->
          Id = {skw:to_atom(proplists:get_value(type, X)),
                proplists:get_value(id, X)},
          Parent = {skw:to_atom(proplists:get_value(parent_type, X)),
                    proplists:get_value(parent_id, X)},
          JSON = proplists:get_value(json, X),
          {Id, Parent, JSON}
      end, SQL),
  list(List).
list([]) -> <<>>;
list(List) -> list(List, []).

list([], Items) -> # list{body = lists:reverse(Items)};
list([Item | T], Items) ->
  {ListItem, Rest} = listitem(Item, T),
  list(Rest, [ListItem | Items]).

listitem({Id, _ParentId, JSON}, List) ->
  Childs = childs(Id, List),
  ChildsCount = length(Childs),
  ObjChildsCount = length([X || {{Type, _}, _, _} = X <- Childs, Type =:= object]),
  Rest = [X || X <- List, not lists:member(X, Childs)],
  Actions = actions(Id),
  DataFields = skw:json_decode(JSON),
  ItemRec = skw:to_rec(JSON),
  Panel = #panel{
      class = skw:class(Id),
      body = proplists:get_value(<<"title">>, DataFields),
%      body = skw:title(ItemRec),
      actions = Actions},
  Body = if
    ChildsCount =:= 0 -> Panel#panel{class = [offset | Panel#panel.class]};
    true ->
      Img = #image{
          anchor = "switch",
          image = "/img/button-open.png"
          },
      [
        Img,
        "&nbsp;",
        Panel,
        list(Childs)
        ]
  end,
  Item = #listitem{
      anchor = skw:class(Id),
      data_fields = [
        {online, 0},
        {closed, false},
        {objects, ObjChildsCount},
        {childs, ChildsCount}
        | DataFields],
      body = Body},
  {Item, Rest}.

childs(_, []) ->
  [];
childs(Id, List) ->
  C = [{ChildId, ChildParent, ChildName}
       || {ChildId, ChildParent, ChildName}
          <- List, ChildParent =:= Id],
  C1 = lists:flatten(lists:map(fun({Id1, _, _}) ->
            childs(Id1, List)
        end, C)),
  C ++ C1.

event(Event) ->
  lager:debug("event ~p", [Event]),
  ok.

drop_event(Id, ParentId) ->
  lager:debug("dropped ~p on ~p", [Id, ParentId]),
  wf:remove(binary_to_list(skw:to_binary(Id))),
  ok.

accept_groups(owner) -> [owner, group, object, terminal];
accept_groups(group) -> [group, object];
accept_groups(object) -> [terminal, sensor, sim];
accept_groups(terminal) -> [sim];
accept_groups(sensor) -> none;
accept_groups(sim) -> none.

actions({object, _} = Id) -> [context_menu(Id), draggable(Id)];
actions(Id) -> [context_menu(Id), droppable(Id), draggable(Id)].

droppable({Type, _}) ->
  #drop{
    accept_groups = accept_groups(Type),
    tolerance = pointer}.
draggable({Type, _}) ->
  #drag{
    delay = 300,
    group = Type,
    revert = false}.
context_menu(_) -> #context_menu{}.
