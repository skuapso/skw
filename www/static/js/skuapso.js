function SkuapsoTerminal() {
  this.new = function(args) {
    var  $args = args || {},
         $dialog = $('div#dialog-terminal'),
         $form = $dialog.find('form')[0],
         $models,
         $i;
    $($form).find('input').each(function() {
      this.value = '';
    });
    for ($i in $args) {
      if ($form[$i]) {
        $form[$i].value = $args[$i];
      }
    };
    $dialog[0].dataset['type'] = 'terminal';
    $dialog[0].dataset['terminal'] = 'null';
    $dialog[0].dataset['model_id'] = '';
    if ($args['tag']) {
      $dialog[0].dataset['tag'] = $args['tag'];
    }
    $models = Skuapso.value($dialog[0].dataset, 'model_id');
    $($form['model_id']).autocomplete({source: $models.autocomplete});
    $('div#dialog-terminal').dialog('open');

  };

  return this;
};

function SkuapsoClass() {
  this.serialized = {};
  this.ui = {};
  this.event = {};
  this.types = ['owner', 'group', 'object'];
  this.object = {};
  this.terminal = new SkuapsoTerminal();

  this.$event = function($serial, $params) {
    Nitrogen.$queue_event(null, null, Skuapso.serialized[$serial], Skuapso.prepare_params($params));
  };
  this.prepare_params = function(data) {
    console.debug('preparing params: %o', data);
    var ser = JSON.stringify(data);
    console.debug('serialized: %o', ser);
    return "data=" + ser;
  };
  this.value = function(_el, _field) {
    var $el = Skuapso.$(_el);
    var
        $type = $el.dataset['type'],
        $val = $el.dataset[_field],
        $match = _field.match(/(.*)_id$/),
        $field = ($match) ? $match[1] : _field;
    if (($match) && (Skuapso[$type]) && (Skuapso[$type][$field])) {
      var $vals_array = Array();
      for (i in Skuapso[$type][$field]) {
        $vals_array.push(Skuapso[$type][$field][i].toString());
      }
      return {
        value: Skuapso[$type][$field][parseInt($val)],
        autocomplete: $vals_array.sort()
      };
    }
    return {value: $val, autocomplete: []};
  };
  this.id = function(_el, _field, _value) {
    var $el = Skuapso.$(_el),
        $type = $el.dataset['type'],
        $match = _field.match(/(.*)_id$/),
        $field = ($match) ? $match[1] : _field;
    if ($match) {
      for (i in Skuapso[$type][$field]) {
        if (Skuapso[$type][$field][i] == _value) {
          return parseInt(i);
        }
      }
      var v = _value;
      _value = Object();
      _value['new'] = {type: $field, title: v};
    }
    return _value;
  }
  this.element = this.$ = function(_el) {
    return $('[data-' + _el.type + '=' + _el[_el.type] + ']')[0];
  }
  this.elements = this.$$ = function(_el) {
    return $('[data-' + _el.type + '=' + _el[_el.type] + ']');
  }
  this.data = function(_el) {
    return Skuapso.$(_el).dataset;
  }
  this.typeSort = function(a, b) {
    var i, l, type;
    l = Skuapso.types.length
    for (i = 0, type=Skuapso.types[i]; i < l; i++, type=Skuapso.types[i]) {
      if (a.s[0] == type) {
        if (b.s[0] == type) {
          return 0;
        }
        return -1;
      }
      if (b.s[0] == type) {
        return 1;
      }
    };
    return 0;
  }
  this.set_title = function(_el) {
    var $els = Skuapso.$$(_el),
        $title = Skuapso.$(_el).dataset['title'];
    $els.find('>div').text($title);
  }

  this.ui.contextMenu = function(ev) {
    var event = $(ev.target).data('action'),
        item = $(Nitrogen.$anchor_path).parent()[0].dataset;
    Skuapso.$event('context_menu',[event, item]);
  };
  this.ui.default = function(ev) {
    var type = $(this).parent()[0].dataset.type;
    $($('menu#' + type + ' command')[0]).trigger('click');
    return false;
  };
  this.ui.drop = function(ev, ui) {
    var
        item = $(ui.draggable[0]).parent()[0].dataset,
        dropped_on = $(this).parent()[0].dataset;
    Skuapso.$event('drop', [item, dropped_on]);
  };
  this.ui.set = function(ev, data) {
    var $el = this.dataset,
        $field = data.field,
        $val = Skuapso.id($el, $field, data.value);
    Skuapso.$event('set', [$field, $val, $el]);
    $(this).trigger('cancel_edit');
    return false;
  };
  this.ui.new = function(tag, data) {
    Skuapso.$event('new', [tag, data]);
  };
  this.ui.cancel = function(tag) {
    Skuapso.$event('cancel', [tag]);
  };

  this.event.move = function(_el, _new_parent) {
    var $el = $(Skuapso.$(_el)),
        $parent = $el.parent().parent(),
        $new_parent = $(Skuapso.$(_new_parent));

    $el.detach();
    $parent.trigger('check-childs');
    if ($parent[0].dataset.childs == '0') {
      var $div = $parent.find('>div').detach();
      $parent.text('');
      $parent.append($div);
    }

    if ($new_parent.find('>ul').length == 0) {
      $new_parent[0].dataset.closed=false;
      $new_parent.prepend('<img src="/img/button-open.png" class="wfid_switch image">&nbsp;');
      $new_parent.append('<ul class="list"></ul>');
    }
    $new_parent.find('>ul').append($el);
    $new_parent.trigger('check-childs');
    $new_parent.trigger('sort', true);
  };
  this.event.edit = function(_el, _field) {
    var $el = $(Skuapso.$(_el)),
        $val = Skuapso.value(_el, _field),
        $div = $el.find('>div'),
        $texts = $div.text().split($val.value),
        $input;
    $texts[1] = ($texts[1]) ? $texts[1] : '';
    $div
      .after('<label class="ui-front">' + $texts[0]
          + '<input type="text" value="' + $val.value + '">'
          + $texts[1] + '</label>')
      .hide(),
    $input = $el.find('>label>input')
      .focus()
      .select()
      .autocomplete({source: $val.autocomplete})
      .on('focusout', function() {
        $(this).trigger('cancel_edit');
        return false;
      })
      .keydown(function(ev) {
        if (ev.keyCode == 13) {
          $(this).trigger('set', {field: _field, value: $(this).val()});
          return false;
        }
        if (ev.keyCode == 27) {$(this).trigger('cancel_edit'); return false;}
      });
  };
  this.event.set = function(_el, _new) {
    var $el = Skuapso.$(_el);
    for (var k in _new) {
      var $old = Skuapso.value(_el, k).value,
        $new, $splitted_title;
      $el.dataset[k] = _new[k];
      if (k != 'title') {
        $new = Skuapso.value(_el, k).value;
        $splitted_title = $el.dataset.title.split($old);
        $el.dataset.title = $splitted_title.join($new);
      }
    }
    Skuapso.set_title(_el);
    Skuapso.$$(_el).parent().trigger('sort');
  };
  return this;
};
var Skuapso = new SkuapsoClass();

for (i in Skuapso.types) {
  var t = Skuapso.types[i], s = 'li[data-type="' + t + '"]>div';
  $.contextMenu(
    {
      selector: s,
      items: $.contextMenu.fromMenu('menu#' + t)
  });
  $('body').delegate(s, 'dblclick', Skuapso.ui.default);
}
$('menu>command').on('click', Skuapso.ui.contextMenu);
$('body').delegate('li[data-childs][data-childs!="0"]>.wfid_switch', 'click', function(ev) {
  var $this = $(this);
  var closed = ($this.parent()[0].dataset.closed == "true");
  $this.parent().find('>ul').toggle('slow');
  var src = (closed) ? '/img/button-open.png' : '/img/button-closed.png';
  $this.attr('src', src);
  $this.parent()[0].dataset.closed = ! closed;
  return false;
});
$('body').delegate('li[data-childs]', 'check-childs', function() {
  this.dataset.childs = $(this).find('li[data-type]>div').length;
  this.dataset.objects = $(this).find('li[data-type][data-type="object"]>div').length;
  this.dataset.online = $(this).find('li[data-type][data-type="object"][data-online!="0"]').length;
});
$('body').delegate('li[data-childs]', 'sort', function(ev, $prevent) {
  $(this).find('>ul>li').tsort(
    {attr: 'data-type', sortFunction: Skuapso.typeSort},
    {attr: 'data-title'});
  if ($prevent) {
    return false;
  }
});
$('body').delegate('li[data-type]', 'set', Skuapso.ui.set);
$('body').delegate('li[data-type]', 'cancel_edit', function() {
  $(this).find('>label').remove();
  $(this).find('>div').show();
//  return false;
});
$.tinysort.defaults.forceStrings = true;
$.tinysort.defaults.cases = false;
$('body').delegate('ul.ui-autocomplete>li', 'click', function() {
  var $text = $(this).find('a').text();
});
$('div#dialog-terminal').dialog({
  autoOpen: false,
  modal: true,
  beforeClose: function() {
    var $tag = this.dataset['tag'],
        $submit = (this.dataset['submit'] && this.dataset['submit'] == 'true'),
        $vals = {},
        $val,
        $i;
    $(this).removeAttr('data-tag');
    for ($i in this.dataset) {
      if ($submit) {
        $val = this.dataset[$i];
        $vals[$i] = JSON.parse($val);
      }
      $(this).removeAttr('data-' + $i);
    }
    if ($submit) {
      Skuapso.ui.new($tag, $vals);
    } else {
      Skuapso.ui.cancel($tag);
    }
  },
  buttons: {
    'Добавить': function() {
      var $elements = $(this).find('form input'), $dialog = this, $tag;
      $elements.each(function() {
        var $val = Skuapso.id($dialog.dataset, this.name, this.value);
        $dialog.dataset[this.name] = JSON.stringify($val);
      });
      $dialog.dataset['submit'] = true;
      $dialog.dataset['type'] = '"terminal"';
      $dialog.dataset['terminal'] = 'null';
      $(this).dialog('close');
    },
    'Отменить': function() {
      $(this).dialog('close');
    }
  }
})
