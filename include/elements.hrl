-include_lib("nitrogen_core/include/wf.hrl").

-record(skw_template,
        {?ELEMENT_BASE(emse_template),
         file = <<>>,
         optional = false,
         bindings = [],
         prefix = if
      ?MODULE=:=ems_index -> <<>>;
      true -> <<?MODULE_STRING>>
    end
        }).
-record(skw_blank, {?ELEMENT_BASE(skw_blank)}).
-record(skw_ni, {?ELEMENT_BASE(skw_ni), msg = ?MODULE}).
-record(owner, {?ELEMENT_BASE(skw_owner), title}).
-record(group, {?ELEMENT_BASE(skw_group), title}).
-record(object, {?ELEMENT_BASE(skw_object), no, specialization_id, model_id, terminal_id}).
-record(object_model, {?ELEMENT_BASE(skw_object), title}).

-record(drop, {?ACTION_BASE(action_drop), accept_groups=all, active_class=active, hover_class=hover, delegate=undefined, tolerance=intersect}).
-record(drag, {?ACTION_BASE(action_drag), group, handle, clone=true, revert=true, scroll=true, container = false, zindex = false, delay = 0}).
-record(context_menu, {?ACTION_BASE(action_context_menu)}).

-record(terminal, {?ELEMENT_BASE(skw_terminal), model_id, uin, serial_no}).
-record(terminal_model, {?ELEMENT_BASE(skw_terminal), title}).
