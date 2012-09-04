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
function build_boxes_r(root_node, tree, last_parent_node, depth) {
    var id_siffix = tree.name+'_'+depth;

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
            build_boxes(root_node, root_node.data('treeroot'));
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
            build_boxes_r(root_node, tree.children[i], node, depth + 1);
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

function build_boxes(div, tree)
{
    div.text('');
    div.data('treeroot', tree);
    build_boxes_r(div, tree, div, 0);
}

function parse_and_update(root_node, qry) {
    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            build_boxes(root_node, pTree);
        }
    });
}

function edit_sql(tblDlg, qry) {
    if(qry == null || qry.length == 0) {
        sql_editor(tblDlg, null, null, "");
        return;
    }

    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error') && pTree.error.length > 0) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            sql_editor(tblDlg, pTree, null, qry);
        }
    });
}

var undefinedTable = "Query";
var undefinedTableIdx = 0;
function sql_editor(tblDlg, tree, pos, qry) {
    var share        = 80; // percent
    var boxHeight    = 500;
    var boxWidth     = 500;
    var visHeight    = Math.round(boxHeight * share / 100);
    var sqlTxtHeight = boxHeight - visHeight;

    var titleStr = "Sql Visualizer";
    if(tblDlg != null && tblDlg != undefined)
        titleStr = tblDlg.dialog("option", "title").text();

    var X = 115, Y = 115;
    if(pos != null) {X = pos.docX; Y = pos.docY;}

    $('<div style="width:100%"></div>')
    .appendTo(document.body)
    .bind("dialogresize", function (event, ui) {
        var dh = $(this).height() - 10;
        var dw = $(this).width() - 8;
        var seh = Math.round(dh * share / 100);
        var sth = dh - seh;
        $(this).children('div').width(dw)
                               .height(seh);
        $(this).children('textarea').width(dw - 4)
                                    .height(sth);
    })
    .dialog({
        autoOpen: false,
        height: 'auto',
        width: 'auto',
        modal: false,
        position: [X, Y],
        resizable: true,
        title: titleStr,
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        open: function(event, ui) {
            if(tree != null)
                build_boxes($(this).children('div'), tree);
        },
        buttons: {
            "Re-Draw": function() {
                qStr = $(this).children('textarea').val().replace(/(\r\n|\n|\r)/gm," ");
                parse_and_update($(this).children('div'), qStr);
            },
            "Ok": function() {
                qStr = $(this).children('textarea').val().replace(/(\r\n|\n|\r)/gm," ");
                $(this).dialog('close');
                if(tblDlg != null && tblDlg != undefined)
                    tblDlg.trigger('requery', qStr);
                else {
                    load_table({name: undefinedTable + undefinedTableIdx + ".sql", content : qStr});
                    ++undefinedTableIdx;
                }
            }
        }
    })
    .append($('<div></div>') // For holding the box
            .width(boxWidth)
            .height(visHeight)
            //.css('background-color', 'rgb(255,255,0)')
            .addClass("ui-widget-content")
            .css("overflow", "auto"))
    .append($('<textarea>'+qry+'</textarea>')
            .width(boxWidth - 4)
            .height(sqlTxtHeight))
    .dialog("open");
}
