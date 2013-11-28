-module(skw_owner).

-compile([export_all]).

-include("elements.hrl").

render_element(#owner{}) -> [].

json2rec(JSON) -> json2rec(JSON, #owner{}).

json2rec([], Owner) -> Owner;
json2rec([{<<"title">>, Title} | T], Owner) ->
  json2rec(T, Owner#owner{title = Title});
json2rec([{<<"owner">>, Id} | T], Owner) ->
  json2rec(T, Owner#owner{id = skw:to_integer(Id)});
json2rec([{_Key, _Val} | T], Owner) ->
  json2rec(T, Owner).

dropped(Item, DroppedOn) ->
  lager:warning("dropping ~p on ~p not implemented", [Item, DroppedOn]).

proplist(#owner{id = Id, title = Title}) ->
  [{type, owner}, {owner, Id}, {title, Title}].

title(#owner{title = Title}) -> Title.
