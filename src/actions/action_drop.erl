-module(action_drop).

-compile([export_all]).

-include("elements.hrl").

render_action(Record) -> 
  % Get properties...
  Anchor = Record#drop.anchor,
  ActiveClass = Record#drop.active_class, 
  HoverClass = Record#drop.hover_class,
  Tolerance = Record#drop.tolerance,
  AcceptGroups = groups_to_accept(Record#drop.accept_groups),

  % Write out the script to make this element droppable...
  wf:f("$('~s').droppable({drop: Skuapso.ui.drop, activeClass: '~s', hoverClass: '~s', tolerance: '~s', accept: '~s'});\n",
       [Anchor, ActiveClass, HoverClass, Tolerance, AcceptGroups]).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
  Groups1 = lists:flatten([Groups]),
  Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
  string:join(Groups2, ", ").
