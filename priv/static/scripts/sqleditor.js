function get_text(box, txt)
{
    txt = (box.name + ' ');
    for(var i=0; i < box.children.length; ++i) {
        txt += get_text(box.children[i].box, txt) + ' ';
    }
    return txt.trim();
}

function prep_box(box)
{
    box['summary'] = get_text(box, '');
    box['issummary'] = true;
    if(!box.hasOwnProperty('height')) box['height'] = 0;
    if(box.name.length > 0)
        box.height = def_height;
    for (var i=0; i < box.children.length ; ++i) {
        box.children[i]['height'] = 0;
        prep_box(box.children[i].box);
        box.height += box.children[i].height; 
    }
}

var def_height = 20;
var tab_len    = 10;
function build_boxes_r(root_node, box, last_parent_node, depth) {
    var id_siffix = box.name+'_'+depth;

    var bgcol = (255 - 10 * depth);
    //var col = (30 * depth);

    // '(' or ')' collapses the parent
    if(box.name == "(" || box.name == ")")
        box.collapsed = false;

    var content = $(('<span>'+box.name+'</span>'));

    var node = (box.name.length > 0
                ? $('<div id="'+id_siffix+'d"></div>').append(content)
                                                      .data("content", content)
                : $('<div></div>'));

    var editbx = $('<span></span>')
        .appendTo(node)
        .css('background-color', 'rgb('+bgcol+','+bgcol+','+bgcol+')')
        .addClass('edit_div')
        .css("font-family","courier")
        .css("font-size","10pt")
        .hide()
        .text(box.summary);

    var txtarea = $('<textarea></textarea>')
        .appendTo(node)
        .css('background-color', 'rgb('+bgcol+','+bgcol+','+bgcol+')')
        .addClass('edit_div')
        .css("font-family","courier")
        .css("font-size","10pt")
        .hide()
        .text(box.summary);

    node.dblclick(function(evt) {
            evt.stopPropagation();
            var parent_box = last_parent_node.data("box");
            box.collapsed = !box.collapsed;
            if(box.name == "(" || box.name == ")") {
                parent_box.collapsed = box.collapsed;
                for (var i =0; i < box.children.length; ++i)
                    box.children[i].collapsed = box.collapsed
            }
            if(box.children.length == 0)
                parent_box.collapsed = !parent_box.collapsed;
            build_boxes(root_node, root_node.data('boxroot'));
        })
        .click(function(evt) {
            evt.stopPropagation();
            var parent_box = last_parent_node.data("box");
            box.collapsed = !box.collapsed;
            if(box.name == "(" || box.name == ")") {
                parent_box.collapsed = box.collapsed;
                for (var i =0; i < box.children.length; ++i)
                    box.children[i].collapsed = box.collapsed
            }
            if(box.children.length == 0)
                parent_box.collapsed = !parent_box.collapsed;
            build_boxes(root_node, root_node.data('boxroot'));
        })
        .addClass('inner_div')
        .addClass('context-menu-one')
        .css('background-color', 'rgb('+bgcol+','+bgcol+','+bgcol+')')
        //.css('color', 'rgb('+col+','+col+','+col+')')
        .data("box", box)
        .css('top', 0)
        .css('left', (depth > 0 ? tab_len : 0));

    last_parent_node.append(node);

    var child_hide = false;
    for (var i=0; i < box.children.length ; ++i) {
        if(box.collapsed) {
            if(!child_hide)
                child_hide = true;
        } else
            build_boxes_r(root_node, box.children[i].box, node, depth + 1);
    }

    if(child_hide) node.height(editbx.height());
    else           node.height(box.height);

    if(box.collapsed && box.issummary) {
        //txtarea.show();
        editbx.show();
        content.hide();
    }
}

//function get_box_height(box)
//{
//    var height = def_height;
//    if(!box.collapsed) {
//        for(var i=0; i < box.children; ++i)
//            height += get_box_height(box.children[i]);
//    }
//    return height;
//}
//
function build_boxes(div, box)
{
    div.text('');
    div.data('boxroot', box);
    build_boxes_r(div, box, div, 0);
}

//function parse_and_update(root_node, qry) {
//    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
//        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
//            alert(pTree.error);
//        } else {
//            prep_box(pTree.box);
//            build_boxes(root_node, pTree);
//        }
//    });
//}
//
//function edit_sql(tblDlg, qry) {
//    if(qry == null || qry.length == 0) {
//        sql_editor(tblDlg, null, null, "");
//        return;
//    }
//
//    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
//        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
//            alert(pTree.error);
//        } else {
//            prep_box(pTree.box);
//            sql_editor(tblDlg, pTree.box, null, qry);
//        }
//    });
//}
//
// var undefinedTable = "Query";
// var undefinedTableIdx = 0;
// function sql_editor(tblDlg, box, pos, qry) {
//     var share        = 80; // percent
//     var boxHeight    = 500;
//     var boxWidth     = 500;
//     var visHeight    = Math.round(boxHeight * share / 100);
//     var sqlTxtHeight = boxHeight - visHeight;
// 
//     var titleStr = "Sql Visualizer";
//     if(tblDlg != null && tblDlg != undefined)
//         titleStr = tblDlg.dialog("option", "title").text();
// 
//     var X = 115, Y = 115;
//     if(pos != null) {X = pos.docX; Y = pos.docY;}
// 
//     $('<div style="width:100%"></div>')
//     .appendTo(document.body)
//     .bind("dialogresize", function (event, ui) {
//         var dh = $(this).height() - 10;
//         var dw = $(this).width() - 8;
//         var seh = Math.round(dh * share / 100);
//         var sth = dh - seh;
//         $(this).children('div').width(dw)
//                                .height(seh);
//         $(this).children('textarea').width(dw - 4)
//                                     .height(sth);
//     })
//     .dialog({
//         autoOpen: false,
//         height: 'auto',
//         width: 'auto',
//         modal: false,
//         position: [X, Y],
//         resizable: true,
//         title: titleStr,
//         close: function() {
//             $(this).dialog('destroy');
//             $(this).remove();
//         },
//         open: function(event, ui) {
//             if(box != null)
//                 build_boxes($(this).children('div'), box);
//         },
//         buttons: {
//             "Re-Draw": function() {
//                 qStr = $(this).children('textarea').val().replace(/(\r\n|\n|\r)/gm," ");
//                 parse_and_update($(this).children('div'), qStr);
//             },
//             "Ok": function() {
//                 qStr = $(this).children('textarea').val().replace(/(\r\n|\n|\r)/gm," ");
//                 $(this).dialog('close');
//                 if(tblDlg != null && tblDlg != undefined)
//                     tblDlg.trigger('requery', qStr);
//                 else {
//                     load_table({name: undefinedTable + undefinedTableIdx + ".sql", content : qStr});
//                     ++undefinedTableIdx;
//                 }
//             }
//         }
//     })
//     .append($('<div></div>') // For holding the box
//             .width(boxWidth)
//             .height(visHeight)
//             //.css('background-color', 'rgb(255,255,0)')
//             .addClass("ui-widget-content")
//             .css("overflow", "auto"))
//     .append($('<textarea>'+qry+'</textarea>')
//             .width(boxWidth - 4)
//             .height(sqlTxtHeight))
//     .dialog("open");
// }
