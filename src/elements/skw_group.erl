-module(skw_group).

-compile([export_all]).

-include("elements.hrl").

render_element(#group{id = Id, title = Title}) -> [].

json2rec(JSON) -> json2rec(JSON, #group{}).

json2rec([], Group) -> Group;
json2rec([{<<"title">>, Title} | T], Group) ->
  json2rec(T, Group#group{title = Title});
json2rec([{<<"group">>, Id} | T], Group) ->
  json2rec(T, Group#group{id = skw:to_integer(Id)});
json2rec([{_Key, _Val} | T], Group) ->
  json2rec(T, Group).

dropped(#group{} = Item, #group{id = Id} = Group) ->
  set(Item, <<"parent_id">>, Id),
  wf:send_global(ui_event, {move, Item, Group});
dropped(#group{} = Item, [{_,_}|_] = DroppedOn) -> dropped(Item, skw:to_rec(DroppedOn));
dropped(Item, DroppedOn) ->
  lager:warning("dropping ~p on ~p not implemented", [Item, DroppedOn]).

proplist(#group{id = Id, title = Title}) ->
  [{type, group}, {group, Id}, {title, Title}].

title(#group{title = Title}) -> Title.

set(#group{id = Id} = Item, Field, Val) ->
  wf:q({sql, execute, {
        skw:join([<<"update objects.groups set">>, Field, <<"=$1 where id=$2">>], " "),
        [Val, Id]}}),
  wf:send_global(ui_event, {set, Item, [{Field, Val}]}).
