
$(function() {
  $hs.init();
  $hs.modules.B.init();
  $hs.modules.Bweb.init()
  $hs.modules.InctimeUtils.init();
  $hs.modules.InctimeHtml.init();
  $hs.loadModule("GHC.Tuple");
  $hs.loadModule("GHC.List");
  $hs.loadModule("GHC.Types");
  $hs.loadModule("Data.Maybe");

  function ensureEval(v) {
    if (v.notEvaluated) {
      return v.evaluate();
    } else {
      return v;
    }
  }

  function fullEval(v) {
    v = ensureEval(v);
    for (var i = 0; v.data != undefined && i < v.data.length; i++) {
      v.data[i] = fullEval(v.data[i]);
    }
    return v;
  }
  
  function jsOutputChange(c) {
    function list_change(member_change, c) {
      c = ensureEval(c);
      if (c.tag == 1) {
        return { type: "Cons change",
                 head: member_change(c.data[0]),
                 tail: list_change(member_change, c.data[1]) };
      } else if (c.tag == 2) {
        return { type: "New head change",
                 new_head: c.data[0] };
      } else if (c.tag == 3) {
        return { type: "New tail change",
                 new_tail: c.data[0] };
      } else if (c.tag == 4) {
        return { type: "Identity" };
      } else if (c.tag == 5) {
        return { type: "Replace",
                 value: c.data[0] };
      } else if (c.tag == 6) {
        return { type: "Empty list change" };
      }
    }
    function char_change(c) {
      c = ensureEval(c);
      if (c.tag == 0) {
        return { type: "primitive char", value: c.data[0] };
      } else if (c.tag == 1) {
        return { type: "replace", value: fullEval(c.data[1]) };
      } else if (c.tag == 2) {
        return { type: "identity" };
      }
    }
    function attr_change(c) {
      return {};
    }
    function string_change(c) {
      return list_change(char_change, c);
    }
    function attrs_change(c) {
      return list_change(attr_change, c);
    }
    function elts_change(c) {
      return list_change(jsOutputChange, c);
    }
    c = ensureEval(c);
    if (c.tag == 1) {
      return { type: "Element change",
               tag_name: string_change(c.data[0]),
               attrs: attrs_change(c.data[1]),
               children: elts_change(c.data[2]) };
    } else if (c.tag == 2) {
      return { type: "Text change",
               change: string_change(c.data[0]) };
    } else if (c.tag == 3) {
      return { type: "IncBox to text change",
               change: fullEval(c.data[0]) };
    } else if (c.tag == 4) {
      return { type: "replace",
               value: fullEval(c.data[0]) };
    } else if (c.tag == 5) {
      return { type: "identity" };
    }
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
      var justValue = $hs.modules.DataziMaybe.hs_Just.hscall(value);
      var v = $hs.modules.GHCziTuple.hs_Z2T.hscall(name, justValue);
      result = hsCons(v, result);
    });
    return result;
  }

  function getDomBox(element) {
    var js = $(element).attr("data-incbox");
    obj = eval("(" + js + ")");
    return obj;
  }

  function setDomBox(element, val) {
    var str = JSON.stringify(val);
    $(element).attr("data-incbox", str);
  }

  function applyBoxInputChange(incbox, val) {
    var applyDict = ensureEval(incbox.data[0]);
    var dataDict = incbox.data[1];
    var f = ensureEval(incbox.data[2]);
    var incval = ensureEval(incbox.data[3]);
    var newBox = $hs.modules.Inctime.hs_applyInputChange.hscall(applyDict,
                                                                incval,
                                                                val);
    console.log("Box", "old", val, "new", ensureEval(newBox));
    console.log(ensureEval(incval),
                ensureEval(incval.data[0]),
                ensureEval(ensureEval(incval.data[0]).data[0]),
                ensureEval(incval.data[1]));
    var newVal = ensureEval(newBox);
    return {innerVal: newVal, outval: $hs.fromHaskellString(f.hscall(newVal))};
  }

  function applyStringChange(s, change) {
    return $hs.modules.Inctime.hs_applyInputChange.hscall(
        $hs.modules.Inctime.hs_zdfApplicableIncrementalisedZMZNBuiltinListzuincrementalised.hscall(
          $hs.modules.Inctime.hs_zdfApplicableIncrementalisedCharCharzuincrementalised),
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
      console.log("elem change", context, index, change,
          context.childNodes, context.childNodes[index], ensureEval(change));
      applyUiChange(context.childNodes[index], change);
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
      case 6: // empty
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
        applyAttrChange(index, head_change);
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
      case 6: // empty
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
        break;
      case 2: // replace
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
      var newText = $hs.fromHaskellString(applyStringChange(hsString(text),
                                          ui_change.data[0]));
      console.log(context, "new text ", newText);
      $(context).text(newText);
      break;
    case 3: // TextElementBox_Incrementalised
      var incbox = ui_change.data[0];
      var boxval = getDomBox(context);
      var newBoxVal = applyBoxInputChange(incbox, boxval);
      setDomBox(context, newBoxVal.innerVal);
      $(context).text(newBoxVal.outval);
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
    console.log("input change", fullEval(input_change));
    var state_change = $hs.modules.B.hs_appzustatezuincrementalised.hscall(input_change);
    console.log("state change", fullEval(state_change));
    var ui_change = $hs.modules.B.hs_pagezuviewzuincrementalised.hscall(state_change);
    console.log("output change", jsOutputChange(ui_change));
    applyUiChange($("#inctime-body")[0].children[0], ui_change);
  });
});

