function get_text(tree, txt)
{
    txt = (tree.name + ' ');
    for(var i=0; i < tree.children.length; ++i) {
        txt += get_text(tree.children[i], txt) + ' ';
    }
    return txt.trim();
}

function prep_tree(tree)
{
    tree['summary'] = get_text(tree, '');
    if(!tree.hasOwnProperty('height')) tree['height'] = 0;
    if(tree.name.length > 0)
        tree.height = def_height;
    for (var i=0; i < tree.children.length ; ++i) {
        tree.children[i]['height'] = 0;
        prep_tree(tree.children[i]);
        tree.height += tree.children[i].height; 
    }
}

var def_height = 20;
var tab_len    = 10;
function build_boxes_r(root_node, dc, tree, last_parent_node, depth) {
    var id_siffix = tree.name+'_'+dc+'_'+'_'+depth;

    var bgcol = (255 - 10 * depth);
    //var col = (30 * depth);

    // '(' or ')' collapses the parent
    if(tree.name == "(" || tree.name == ")")
        tree.collapsed = false;

    var content = $(('<span>'+tree.name+'</span>'));

    var node = (tree.name.length > 0
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
        .text(tree.summary);

    node.dblclick(function(evt) {
            event.stopPropagation();
            var parent_tree = last_parent_node.data("tree");
            tree.collapsed = !tree.collapsed;
            if(tree.name == "(" || tree.name == ")") {
                parent_tree.collapsed = tree.collapsed;
                for (var i =0; i < tree.children.length; ++i)
                    tree.children[i].collapsed = tree.collapsed
            }
            if(tree.children.length == 0)
                parent_tree.collapsed = !parent_tree.collapsed;
            build_boxes(root_node, dc, root_node.data('treeroot'));
        })
        .addClass('inner_div')
        .addClass('context-menu-one')
        .css('background-color', 'rgb('+bgcol+','+bgcol+','+bgcol+')')
        //.css('color', 'rgb('+col+','+col+','+col+')')
        .data("tree", tree)
        .data("edit", editbx)
        .css('top', 0)
        .css('left', (depth > 0 ? tab_len : 0));

    last_parent_node.append(node);

    var child_hide = false;
    for (var i=0; i < tree.children.length ; ++i) {
        if(tree.collapsed) {
            if(!child_hide)
                child_hide = true;
        } else
            build_boxes_r(root_node, dc, tree.children[i], node, depth + 1);
    }

    if(child_hide) node.height(editbx.height());
    else           node.height(tree.height);

    if(tree.collapsed) {
        editbx.show();
        content.hide();
    }
}

function get_tree_height(tree)
{
    var height = def_height;
    if(!tree.collapsed) {
        for(var i=0; i < tree.children; ++i)
            height += get_tree_height(tree.children[i]);
    }
    return height;
}

function build_boxes(div, dc, tree)
{
    div.text('');
    div.data('treeroot', tree);
    build_boxes_r(div, dc, tree, div, 0);
}

function parse_and_update(root_node, qry, dc) {
    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            build_boxes(root_node, dc, pTree);
        }
    });
}

function edit_sql(tblDlg, qry) {
    if(qry == null || qry.length == 0) {
        sql_editor(tblDlg, 0, null, null, "");
        return;
    }

    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            sql_editor(tblDlg, 0, pTree, null, qry);
        }
    });
}

var undefinedTable = "Query";
var undefinedTableIdx = 0;
function sql_editor(tblDlg, dc, tree, pos, qry) {
    var share        = 80; // percent
    var boxHeight    = 500;
    var boxWidth     = 500;
    var visHeight    = Math.round(boxHeight * share / 100);
    var sqlTxtHeight = boxHeight - visHeight;
    $('<div id="pick_conds'+dc+'" style="width:100%">'+
            '<div id="cond_viz'+dc+'" >'+
            '<div id="sql_tree'+dc+'"></div>'+
            '</div>'+
            '<textarea id="pick_conds_str'+dc+'">'+qry+'</textarea>'+
      '</div>'
    ).appendTo(document.body);

    $('#cond_viz'+dc)
        .width(boxWidth)
        .height(visHeight)
        //.css('background-color', 'rgb(255,255,0)')
        .addClass("ui-widget-content")
        .css("overflow", "auto");

    $('#pick_conds_str'+dc)
        .width(boxWidth - 4)
        .height(sqlTxtHeight);

    $('#pick_conds'+dc).bind("dialogresize", function (event, ui) {
        var dh = $('#pick_conds'+dc).height() - 10;
        var dw = $('#pick_conds'+dc).width() - 8;
        var seh = Math.round(dh * share / 100);
        var sth = dh - seh;
        $('#cond_viz'+dc).width(dw)
                         .height(seh);
        $('#pick_conds_str'+dc).width(dw - 4)
                               .height(sth);
    });
    
    if(tree != null)
        build_boxes($('#sql_tree'+dc), dc, tree);

    var titleStr = "Sql Visualizer";
    if(tblDlg != null && tblDlg != undefined)
        titleStr = tblDlg.dialog("option", "title").text();

    var X = 115, Y = 115;
    if(pos != null) {X = pos.docX; Y = pos.docY; }
    $('#pick_conds'+dc).dialog({
        autoOpen: false,
        height: 'auto',
        width: 'auto',
        modal: false,
        position: [X, Y],
        resizable: true,
        title: titleStr,
        close: function() {
            $('#pick_conds'+dc).dialog('destroy');
            $('#pick_conds'+dc).remove();
        },
        buttons: {
            "Save": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                undefinedTableIdx = 0;
                ajax_post("/app/save_file", {save: {file_name:titleStr, file_content:qStr}}, null, null, null);
                $(this).dialog('close');
            },
            "Save As": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                undefinedTableIdx = 0;
                $('<div><input id="saveas_'+dc+'" type="text" value="'+undefinedTable + undefinedTableIdx + '.sql"/>'
                    +'</div>')
                    .appendTo(document.body)
                    .dialog({
                        autoOpen: false,
                        height: 105,
                        width: 'auto',
                        modal: true,
                        resizable: false,
                        title: "Save SQL as",
                        close: function() {
                            $(this).dialog('destroy');
                            $(this).remove();
                        },
                        buttons: {
                            "Ok": function() {
                                var fileName = $('#saveas_'+dc).val();
                                ajax_post("/app/save_file", {save: {file_name:FileName, file_content:qStr}}, null, null, null);
                                $(this).dialog('close');
                            }
                        }})
                    .dialog("open");
            },
            "Re-Draw": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                parse_and_update($('#sql_tree'+dc), qStr, dc);
            },
            "Cancel": function() {
                $('#pick_conds'+dc).dialog('close');
            },
            "Ok": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                $('#pick_conds'+dc).dialog('close');
                if(tblDlg != null && tblDlg != undefined)
                    tblDlg.trigger('requery', qStr);
                else {
                    load_table(undefinedTable + undefinedTableIdx + ".sql", qStr);
                    ++undefinedTableIdx;
                }
            }
        }
    });
    $('#pick_conds'+dc).dialog("open");
}
