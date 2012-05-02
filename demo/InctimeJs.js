$(function() {
  $hs.init();
  $hs.modules.B.init();
  $hs.modules.Bweb.init()
  $hs.modules.InctimeUtils.init();
  $hs.modules.InctimeHtml.init();
  $hs.loadModule("GHC.Tuple");
  $hs.loadModule("GHC.List");
  $hs.loadModule("GHC.Types");

  function ensureEval(v) {
    if (v.notEvaluated)
      return v.evaluate();
    else
      return v;
  }

  function hsEmpty() {
    return $hs.modules.GHCziTypes.hs_ZMZN.hscall();
  }
  function hsCons(h, t) {
    return $hs.modules.GHCziTypes.hs_ZC.hscall(h, t);
  }

  function hsString(s) {
    var h = hsEmpty();
    for (var i = s.length - 1; i >= 0; i--) {
      var c = $hs.modules.GHCziTypes.hs_Czh.hscall(s[i]);
      h = hsCons(c, h);
    }
    return h;
  }

  function query() {
    var result = hsEmpty();
    $("input").each(function() {
      if (this.name == "" || this.value == "")
        return;
      var name = hsString(this.name);
      var value = hsString(this.value);
      var v = $hs.modules.GHCziTuple.hs_Z2T.hscall(name, value);
      result = hsCons(v, result);
    });
    return result;
  }

  function applyStringChange(s, change) {
    return $hs.modules.Radtime.hs_applyInputChange.hscall(
        $hs.modules.Radtime.hs_zdfIncrementalisedZZMZZNzuincrementalisedZMZN.hscall(
          $hs.modules.Radtime.hs_zdfIncrementalisedCharzuincrementalisedChar),
        change,
        s);
  }

  function applyUiChange(context, ui_change) {
    function jsAttr(attr) {
      var attr = ensureEval(attr);
      var name = $hs.fromHaskellString(attr.data[0]);
      var value = $hs.fromHaskellString(attr.data[1]);
      return {name: name, value: value};
    }

    function applyElemChange(index, change) {
      console.log(context, index, change);
      console.log($(context).children());
      console.log($(context).children()[index]);
      applyUiChange($(context).children()[index], change);
    }

    function applyChildrenChange(index, children_change) {
      children_change = ensureEval(children_change);

      switch (children_change.tag) {
      case 1: // ZMZN_incrementalised
        var head_change = children_change.data[0];
        var tail_change = children_change.data[1];
        applyElemChange(index, head_change)
        applyChildrenChange(index + 1, tail_change);
        break;
      case 2: // build_using_1 (new head)
        throw "applyUiChange Children build 1";
        break;
      case 3: // build_using_2 (weirdy daft)
        throw "applyUiChange Children build 2";
        break;
      case 4: // identity
        // do nothing
        break;
      case 5: // replace
        throw "applyUiChange Children replace";
        break;
      case 6: // hoist
        throw "applyUiChange Children hoist";
        break;
      case 7: // empty
        // do nothing
        break;
      }
    }

    function applyAttrsChange(index, attrs_change) {
      attrs_change = ensureEval(attrs_change);

      switch (attrs_change.tag) {
      case 1: // ZMZN_incrementalised
        var head_change = attrs_change.data[0];
        var tail_change = attrs_change.data[1];
        applyAttrChange(index, head_change)
        applyAttrsChange(index + 1, tail_change);
        break;
      case 2: // build_using_1 (new head)
        throw "applyUiChange Attrs build 1";
        var attr = jsAttr(attrs_change.data[0]);
        $(context).attr(attr.name, attr.value);
        break;
      case 3: // build_using_2 (weirdy daft)
        throw "applyUiChange Attrs build 2";
        break;
      case 4: // identity
        // do nothing
        break;
      case 5: // replace
        throw "applyUiChange Attrs replace";
        for (var i = 0; i < context.attributes.length; i++) {
          var attr = context.attributes[i];
          $(context).removeAttr(attr.name);
        }
        var newAttributes = ensureEval(attrs_change.data[0]);
        while (newAttributes.data.length == 2) {
          var attr = jsAttr(newAttributes.data[0]);
          $(context).attr(attr.name, attr.value);
          newAttributes = newAttributes.data[1];
        }
        break;
      case 6: // hoist
        throw "applyUiChange Attrs hoist";
        break;
      case 7: // empty
        // do nothing
        break;
      }
    }

    function applyAttrChange(index, attr_change) {
      switch (attr_change.tag) {
      case 1: // Attr_incrementalised
        var name_change = attr_change.data[0];
        var value_change = attr_change.data[1];
        var name = context.attributes[index].name;
        var value = context.attributes[index].value;
        var newName = $hs.fromHaskellString(applyStringChange(hsString(name), name_change));
        if (newName != name)
          throw "applyUiChange Attr main got name change";
        var newValue = $hs.fromHaskellString(applyStringChange(hsString(value), value_change));
        context.attributes[index].value = newValue;
        throw "applyUiChange Attr main";
        break;
      case 2: // hoist
        throw "applyUiChange Attr hoist";
        break;
      case 3: // replace
        var newAttr = jsAttr(attr_change.data[0]);
        if (newAttr.name != context.attributes[index].name)
          throw "applyUiChange Attr replace got name change";
        context.attributes[index].value = newAttr.value;
        break;
      }
    }

    ui_change = ensureEval(ui_change);
    switch (ui_change.tag) {
    case 1: // Element_incrementalised
      var tag_name_change = ui_change.data[0];
      var attrs_change = ui_change.data[1];
      var children_change = ui_change.data[2];
      applyAttrsChange(0, attrs_change);
      applyChildrenChange(0, children_change);
      break;
    case 2: // TextElement_incrementalised
      var text = $(context).text();
      console.log(text);
      var newText = $hs.fromHaskellString(applyStringChange(hsString(text), ui_change.data[0]));
      $(context).text(newText);
      break;
    case 3: // hoist
      // do nothing throw "applyUiChange hoist";
      break;
    case 4: // replace
      var newDom = ui_change.data[0];
      var newMarkup = $hs.modules.InctimeUtils.hs_renderHtml.hscall(newDom);
      $(context).replaceWith($($hs.fromHaskellString(newMarkup)));
      break;
    }
  }

  $("input[type=submit]").click(function(e) {
    e.preventDefault();
    var input_change = $hs.modules.Bweb.hs_parsezurequest.hscall(query());
    var state_change = $hs.modules.B.hs_appzustatezuincrementalised.hscall(input_change);
    var ui_change = $hs.modules.B.hs_pagezuviewzuincrementalised.hscall(state_change);
    applyUiChange($("body")[0], ui_change);
  });
});
