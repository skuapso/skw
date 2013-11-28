-module(action_drag).

-compile([export_all]).

-include("elements.hrl").

render_action(Record) -> 
  % Get properties...
  Anchor = Record#drag.anchor,
  GroupClasses = groups_to_classes(Record#drag.group),

  Handle = case Record#drag.handle of
    undefined -> "null";
    Other2 -> wf:f("'.~s'", [Other2])
  end,

  Helper = case Record#drag.clone of
    true -> clone;
    false -> original
  end,

  Revert = case Record#drag.revert of
    true -> "true";
    false -> "false";
    valid -> "'valid'";
    invalid -> "'invalid'"
  end,

  Container = case Record#drag.container of
    false -> false;
    window -> "'window'";
    parent -> "'parent'";
    document -> "'document'";
    V -> V
  end,

  % Write out the script to make this element draggable...
  [wf:f(
      "$('~s').draggable({ handle: ~s, helper: '~s', revert: ~s, scroll: ~s, containment: ~s, zIndex: ~p, appendTo: 'body', delay: ~p });\n",
      [
        Anchor,
        Handle,
        Helper, 
        Revert, 
        Record#drag.scroll,
        Container,
        Record#drag.zindex,
        Record#drag.delay
        ]),
   #add_class{ class=GroupClasses}
  ].

groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
  Groups1 = lists:flatten([Groups]),
  Groups2 = ["drag_group_" ++ wf:to_list(X) || X <- Groups1],
  string:join(Groups2, " ").
