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
    tree['display'] = true;
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

    var content = $(('<span>'+tree.name+'</span>'));

    var node = (tree.name.length > 0
                ? $('<div id="'+id_siffix+'d"></div>').append(content)
                                                      .data("content", content)
                : $('<div></div>'));

    var editbx = $('<textarea></textarea>')
        .appendTo(node)
        .addClass('edit_div')
        .css("font-family","courier")
        .css("font-size","10pt")
        .hide()
        .text(tree.summary);

    node.dblclick(function(evt) {
            event.stopPropagation();
            var dom = $(this).data("tree");
            for(var i=0; i < dom.children.length; ++i)
                dom.children[i].display = !dom.children[i].display;
            build_boxes(root_node, dc, root_node.data('treeroot'));
        })
        .addClass('inner_div')
        .addClass('context-menu-one')
        .toggle(tree.display)
        .css('background-color', 'rgb('+bgcol+','+bgcol+','+bgcol+')')
        //.css('color', 'rgb('+col+','+col+','+col+')')
        .data("tree", tree)
        .data("edit", editbx)
        .css('top', 0)
        .css('left', (depth > 0 ? tab_len : 0));

    last_parent_node.append(node);

    var child_hide = false;
    for (var i=0; i < tree.children.length ; ++i) {
        if(!tree.children[i].display && !child_hide) child_hide = true;
        build_boxes_r(root_node, dc, tree.children[i], node, depth + 1);
    }

    if(child_hide) node.height(node.data("edit").height());
    else           node.height(tree.height);

    if(!tree.display && last_parent_node.data("edit") != undefined) {
        var edit = last_parent_node.data("edit").show();
        var c = last_parent_node.data("content");
        if(c != undefined) c.hide();
    }
}

function build_boxes(div, dc, tree)
{
    div.text('');
    div.data('treeroot', tree);
    build_boxes_r(div, dc, tree, div, 0);
}

function parse_and_update(root_node, qry, dc) {
    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error')) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            build_boxes(root_node, dc, pTree);
        }
    });
}

function edit_sql(tblDlg, qry) {
    //*
    if(qry == null || qry.length == 0) {
        sql_editor(tblDlg, 0, null, null, "");
        return;
    }

    ajax_post("/app/parse_stmt", {parse_stmt: {qstr:qry}}, null, null, function(pTree) {
        if (pTree.hasOwnProperty('error')) {
            alert(pTree.error);
        } else {
            prep_tree(pTree);
            sql_editor(tblDlg, 0, pTree, null, qry);
        }
    });
    /*/
  	pTree =JSON.parse(
'{"name":"select", "children":['
+	'{"name":"", "children":['
+		'{"name":"a", "children":[]},'
+		'{"name":",", "children":[]},'
+		'{"name":"b as bb", "children":[]},'
+		'{"name":",", "children":[]},'
+		'{"name":"c", "children":[]}'
+	']},'
+	'{"name":"from", "children":['
+		'{"name":"abc", "children":[]},'
+		'{"name":",", "children":[]},'
+		'{"name":"def", "children":[]}'
+	']},'
+	'{"name":"where", "children":['
+		'{"name":"", "children":['
+			'{"name":"a", "children":[]},'
+			'{"name":"=", "children":[]},'
+			'{"name":"b", "cildren":[]}'
+		']},'
+		'{"name":"or", "children":['
+			'{"name":"", "children":['
+				'{"name":"nvl", "children":['
+					'{"name":"(", "children":['
+						'{"name":"a", "children":[]},'
+						'{"name":",", "children":[]},'
+						'{"name":"0", "children":[]}'
+					']},'
+					'{"name":")", "children":[]}'
+				']}'
+			']},'
+			'{"name":"=", "children":[]},'
+			'{"name":"0", "children":[]}'
+		']}'
+	']}'
+']}'
pTree = JSON.parse(
                        '{"name":"select", "children":['
                        +	'{"name":"", "children":['
                        +		'{"name":"a", "children":[]},'
                        +		'{"name":", b", "children":[]},'
                        +		'{"name":", c", "children":[]}'
                        +	']},'
                        +	'{"name":"from", "children":['
                        +		'{"name":"abc", "children":[]},'
                        +		'{"name":", def", "children":[]}'
                        +	']},'
                        +	'{"name":"where", "children":['
                        +		'{"name":"", "children":['
                        +			'{"name":"a", "children":[]},'
                        +			'{"name":"=", "children":[]},'
                        +			'{"name":"b", "children":[]}'
                        +		']},'
                        +		'{"name":"or", "children":['
                        +			'{"name":"c", "children":[]},'
                        +			'{"name":"=", "children":[]},'
                        +			'{"name":"d", "children":[]}'
                        +		']}'
                        +	']}'
                        +']}'
    );
    prep_tree(pTree);
    qry = "\r\nselect \r\n\ta\r\n\t,b\r\n\t,c\r\nfrom \r\n\tabc\r\n\t, def\r\nwhere\r\n\t\ta\r\n\t\t=\r\n\t\tb\r\n\tor\r\n\t\tc\r\n\t\t=\r\n\t\td\r\n";
    sql_editor(null, 0, pTree, null, qry);
    //*/
}

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
        .css('background-color', 'rgb(255,255,0)')
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

    var X = 115, Y = 115;
    if(pos != null) {X = pos.docX; Y = pos.docY; }
    $('#pick_conds'+dc).dialog({
        autoOpen: false,
        height: 'auto',
        width: 'auto',
        modal: true,
        position: [X, Y],
        resizable: true,
        title: "Sql Visualizar",
        close: function() {
            $('#pick_conds'+dc).dialog('destroy');
            $('#pick_conds'+dc).remove();
        },
        buttons: {
            "Re-Draw": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                parse_and_update($('#sql_tree'+dc), qStr, dc);
            },
            "Re-Query": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                parse_and_update($('#sql_tree'+dc), qStr, dc);
                if(tblDlg != null && tblDlg != undefined)
                    tblDlg.trigger('requery', qStr);
            },
            "Cancel": function() {
                $('#pick_conds'+dc).dialog('close');
            },
            "Ok": function() {
                qStr = $('#pick_conds_str'+dc).val().replace(/(\r\n|\n|\r)/gm," ");
                $('#pick_conds'+dc).dialog('close');
                if(tblDlg != null && tblDlg != undefined)
                    tblDlg.trigger('requery', qStr);
            }
        }
    });
    $('#pick_conds'+dc).dialog("open");
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// OLD UNUSED CODE
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var selected = {
    tables: new Array(),
    fields: new Array(),
    sorts: new Array(),
    conds: new Array(),
    joins: new Array()
};

function load_fields(fields_target) {
    var tabs = {};
    var owns = {};
    for(var i = 0; i < selected.tables.length; ++i) {
        var l = selected.tables[i].split('.');
        tabs[l[1]]=0;
        owns[l[0]]=0;
    }
    var tables = []; for (var i in tabs) tables.push(i);
    var owners = []; for (var i in owns) owners.push(i);
    get_columns(function(data) {
        var colRows = data.rows;
        for(var i = 0; i < colRows.length; ++i)
            $('<option value="'+colRows[i][0]+'">'+colRows[i][0]+'</option>')
                .dblclick(function() {
                    load_tables($(this).data('usr'),tbl_target);
                })
                .appendTo(fields_target);
    });
}

function get_columns(success) {
    var tabs = {};
    var owns = {};
    for(var i = 0; i < selected.tables.length; ++i) {
        var l = selected.tables[i].split('.');
        tabs[l[1]]=0;
        owns[l[0]]=0;
    }
    var tables = []; for (var i in tabs) tables.push(i);
    var owners = []; for (var i in owns) owners.push(i);

    ajax_post('/app/columns', {cols: {tables: tables, owners: owners}}, null, null, success);
}

function load_users(usr_target, tbl_target) {
    ajax_post('/app/users', null, null, null, function(data) {
        var userRows = data.rows;
        var usr = '';
        for(var i = 0; i < userRows.length; ++i) {
            usr = userRows[i][0];
            $('<option value="'+usr+'" '+(usr==owner?'selected':'')+'>'+usr+'</option>')
                .dblclick(function() {
                    load_tables($(this).data('usr'),tbl_target);
                })
                .data('usr',usr)
                .appendTo(usr_target);
        }
    });

    load_tables(tbl_target);
}

function load_tables(target) {
    ajax_post('/app/tables', {tables: {owner: owner}}, null, null, function(data) {
        var tableRows = data.rows;
        var titles = new Array();
        var title = '';
        var maxStrLength = 0;
        for(var i = 0; i < tableRows.length; ++i) {
                title = tableRows[i][0];
                titles[titles.length] = {title: title, target: owner + '.' + tableRows[i][0]};
        }
        ajax_post('/app/views', {views: {owner: owner}}, null, null, function(data) {
            var viewRows = data.rows;
            for(var i = 0; i < viewRows.length; ++i) {
                title = viewRows[i][0];
                titles[titles.length] = {title: title, target: owner + '.' + viewRows[i][0]};
            }
            target.html('');
            for(var i = 0; i < titles.length; ++i) {
                $('<option value="'+titles[i].title+'" '+(i==0?'selected':'')+'>'+titles[i].title+'</option>')
                    .data("tbl", titles[i])
                    .appendTo(target);
            }
        });
    });
}

function eliminateDuplicates(arr, field) {
    var i,
        len=arr.length,
        out=[],
        obj={};
    
    for (i=0;i<len;i++)
        if (field == null)
            obj[arr[i]]=i;
        else
            obj[arr[i][field]]=i;
    for (i in obj)        out.push(arr[obj[i]]);
    
    return out;
}

function reload_selection_sorts() {
    $('#sel_srt_sel_list').html('');
    selected.sorts = eliminateDuplicates(selected.sorts, "txt");
    sqlObj.sorts = [];
    for(var i = 0; i<selected.sorts.length; ++i) {
        sqlObj.sorts[sqlObj.sorts.length] = selected.sorts[i];
        $('<option value="'+selected.sorts[i].txt+'">'+(selected.sorts[i].dir == 0 ? '&uarr; ' : '&darr; ')+selected.sorts[i].txt+'</option>')
            .appendTo('#sel_srt_sel_list');
    }
}

function reload_selection_fields() {
    $('#sel_fld_sel_list').html('');
    selected.fields = eliminateDuplicates(selected.fields, null);
    sqlObj.fields = [];
    for(var i = 0; i<selected.fields.length; ++i) {
        sqlObj.fields[sqlObj.fields.length] = selected.fields[i];
        $('<option value="'+selected.fields[i]+'">'+selected.fields[i]+'</option>')
            .appendTo('#sel_fld_sel_list');
    }
}

function reload_selection_table() {
    $('#sel_tbl_list').html('');
    selected.tables = eliminateDuplicates(selected.tables, null);
    sqlObj.tables = [];
    for(var i = 0; i<selected.tables.length; ++i) {
        sqlObj.tables[sqlObj.tables.length] = selected.tables[i];
        $('<option value="'+selected.tables[i]+'">'+selected.tables[i]+'</option>')
            .appendTo('#sel_tbl_list');
    }
    if(selected.tables.length > 1) {
       $('#join_inp').show();
       $('#sql_edit').height(500);
    }
    else {
       $('#join_inp').hide();
       $('#sql_edit').height(400);
    }
}

function sql_sorts() {
    $('<div id=pick_srts style="width:100%">'+
            '<table width=100% height=100% border=0>'+
                '<tr><td valign="bottom" height=10px>Fields</td>'+
                    '<td valign="top"></td>'+
                    '<td valign="bottom">Select List</td>'+
                    '</tr>'+
                '<tr><td valign="top"><select id=sel_srt_list style="width:100%; height:100%" size=7/></td>'+
                    '<td id="txfr_btns" valign="center" align="center" width=20px></td>'+
                    '<td valign="top"><select id=sel_srt_sel_list style="width:100%; height:100%" size=7/></td>'+
                    '</tr>'+
                '<tr><td valign="bottom" height=10px>Expression</td>'+
                    '<td valign="bottom" align="center" colspan=2><button>Replace</button><button>Insert</button></td>'+
                    '</tr>'+
                '<tr><td valign="top" colspan=3><select id=sel_exprn_list style="width:100%; height:100%"/></td>'+
                    '</tr>'+
                '<tr><td valign="top" height=10px><button>Add Function</button></td>'+
                    '<td valign="top"></td>'+
                    '<td valign="top"></td>'+
                    '</tr>'+
            '</table>'+
      '</div>'
    ).appendTo(document.body);
    load_fields($('#sel_srt_list'));

    $('<button style="width:50px">-&gt;</button>')
        .click(function(){
            $("#sel_srt_list option:selected").each(function () {
                selected.sorts[selected.sorts.length] = {txt:"", dir:0};
                selected.sorts[selected.sorts.length - 1].txt = $(this).text();
            });
            reload_selection_sorts();
        })
        .appendTo($('#txfr_btns'));
    $('<br>').appendTo($('#txfr_btns'));
    $('<button style="width:50px">-&gt;&gt;</button>')
        .click(function(){
            selected.sorts = [];
            $("#sel_srt_list").children().each(function () {
                selected.sorts[selected.sorts.length] = {txt:"", dir:0};
                selected.sorts[selected.sorts.length - 1].txt = $(this).text();
            });
            reload_selection_sorts();
        })
        .appendTo($('#txfr_btns'));

    $('#pick_srts').dialog({
        autoOpen: false,
        height: 300,
        width: 400,
        modal: true,
        position: [115, 115],
        resizable: false,
        title: "Sort Order",
        close: function() {
            $('#pick_srts').dialog('destroy');
            $('#pick_srts').remove();
        },
        buttons: {
            "Delete": function() {
                $("#sel_srt_sel_list option:selected").each(function () {
                    for(var i=0;i<selected.sorts.length;++i) {
                        if(selected.sorts[i].txt == $(this).text())
                            selected.sorts.splice(i,1);
                    }
                    reload_selection_sorts();
                });
            },
            "Cancel": function() {
                $('#pick_srts').dialog('close');
            },
            "Ok": function() {
                load_srt_div($('#sql_edit_srt'), selected.sorts);
                $('#pick_srts').dialog('close');
            },
            "Ascending": function() {
                $("#sel_srt_sel_list option:selected").each(function () {
                    for(var i=0;i<selected.sorts.length;++i) {
                        if(selected.sorts[i].txt == $(this).val())
                            selected.sorts[i].dir = 0;
                    }
                    reload_selection_sorts()
                });
            },
            "Descending": function() {
                $("#sel_srt_sel_list option:selected").each(function () {
                    for(var i=0;i<selected.sorts.length;++i) {
                        if(selected.sorts[i].txt == $(this).val())
                            selected.sorts[i].dir = 1;
                    }
                    reload_selection_sorts();
                });
            }
        }
    });
    $('#pick_srts').dialog("open");
}

function sql_fields() {
    $('<div id=pick_flds style="width:100%">'+
            '<table width=100% height=100% border=0>'+
                '<tr><td valign="bottom" height=10px>Fields</td>'+
                    '<td valign="top"></td>'+
                    '<td valign="bottom">Select List</td>'+
                    '</tr>'+
                '<tr><td valign="top"><select id=sel_fld_list style="width:100%; height:100%" size=7/></td>'+
                    '<td id="txfr_btns" valign="center" align="center" width=20px></td>'+
                    '<td valign="top"><select id=sel_fld_sel_list style="width:100%; height:100%" size=7/></td>'+
                    '</tr>'+
                '<tr><td valign="bottom" height=10px>Expression</td>'+
                    '<td valign="bottom" align="center" colspan=2><button>Replace</button><button>Insert</button></td>'+
                    '</tr>'+
                '<tr><td valign="top" colspan=3><select id=sel_exprn_list style="width:100%; height:100%"/></td>'+
                    '</tr>'+
                '<tr><td valign="top" height=10px><button>Add Function</button></td>'+
                    '<td valign="center" align="right"><input type="checkbox"></td>'+
                    '<td valign="center" align="left">Only return distinct records</td>'+
                    '</tr>'+
            '</table>'+
      '</div>'
    ).appendTo(document.body);
    load_fields($('#sel_fld_list'));

    $('<button style="width:50px">-&gt;</button>')
        .click(function(){
            $("#sel_fld_list option:selected").each(function () {
                selected.fields[selected.fields.length] = $(this).text();
            });
            reload_selection_fields();
        })
        .appendTo($('#txfr_btns'));
    $('<br>').appendTo($('#txfr_btns'));
    $('<button style="width:50px">-&gt;&gt;</button>')
        .click(function(){
            selected.fields = [];
            $("#sel_fld_list").children().each(function () {
                selected.fields[selected.fields.length] = $(this).text();
            });
            reload_selection_fields();
        })
        .appendTo($('#txfr_btns'));

    $('#pick_flds').dialog({
        autoOpen: false,
        height: 300,
        width: 400,
        modal: true,
        position: [115, 115],
        resizable: false,
        title: "Fields",
        close: function() {
            $('#pick_flds').dialog('destroy');
            $('#pick_flds').remove();
        },
        buttons: {
            "Delete": function() {
                $("#sel_fld_sel_list option:selected").each(function () {
                    for(var i=0;i<selected.fields.length;++i) {
                        if(selected.fields[i] == $(this).text())
                            selected.fields.splice(i,1);
                    }
                    reload_selection_fields();
                });
            },
            "Cancel": function() {
                $('#pick_flds').dialog('close');
            },
            "Ok": function() {
                load_div($('#sql_edit_flds'), selected.fields);
                $('#pick_flds').dialog('close');
            }
        }
    });
    $('#pick_flds').dialog("open");
}

function sql_tables() {
    $('<div id=pick_tbls style="width:100%">'+
            '<table width=100% height=100% border=0>'+
                '<tr><td valign="top">Table:</td>'+
                    '<td width=30% valign="top">User Name List</td>'+
                    '<td valign="top"></td>'+
                    '<td width=30% valign="top">Selected Tables</td></tr>'+
                '<tr><td valign="top"><input type=text/></td>'+
                    '<td id=sel_user valign="top"></td>'+
                    '<td></td>'+
                    '<td valign="top" rowspan=2><select id=sel_tbl_list style="width:100%; height:100%" size=7/></td></tr>'+
                '<tr><td valign="top"><select id=tables_list style="width:100%; height:100%" size=7/></td>'+
                    '<td valign="top"><select id=users_list style="width:100%; height:100%" size=7/></td>'+
                    '<td id=btn_td valign="center" align="center"></td>'+
                    '</tr>'+
                '<tr><td valign="top">Source:</td>'+
                    '<td valign="top">Databases:</td>'+
                    '<td></td>'+
                    '<td width=100%></td>'+
                    '</tr>'+
                '<tr><td valign="top"><select id=src_list style="width:100%; height:100%" disabled/></td>'+
                    '<td valign="top" colspan=3><select id=db_list style="width:100%; height:100%"/></td>'+
                    '</tr>'+
                '<tr><td align="right">Expression<br><button disabled>Replace</button><button disabled>Insert</button></td>'+
                    '<td colspan=3 align="center"><input type=text size=50/></td>'+
                    '</tr>'+
            '</table>'+
      '</div>'
    ).appendTo(document.body);
    load_users($('#users_list'),$('#tables_list'));

    $('<button>-&gt;</button>')
        .appendTo('#btn_td')
        .click(function(){
            $("#tables_list option:selected").each(function () {
                selected.tables[selected.tables.length] = $(this).data("tbl").target;
            });
            reload_selection_table();
        });

    $('#pick_tbls').dialog({
        autoOpen: false,
        height: 350,
        width: 550,
        modal: true,
        position: [115, 115],
        resizable: false,
        title: "Tables",
        close: function() {
            $('#pick_tbls').dialog('destroy');
            $('#pick_tbls').remove();
        },
        buttons: {
            "Delete": function() {
                $("#sel_tbl_list option:selected").each(function () {
                    for(var i=0;i<selected.tables.length;++i) {
                        if(selected.tables[i] == $(this).text())
                            selected.tables.splice(i,1);
                    }
                    reload_selection_table();
                });
            },
            "Cancel": function() {
                $('#pick_tbls').dialog('close');
            },
            "Ok": function() {
                load_div($('#sql_edit_tbls'), selected.tables);
                $('#pick_tbls').dialog('close');
            }
        }
    });
    $('#pick_tbls').dialog("open");
}

function load_srt_div(target, array) {
    var iHtml = '';
    for(var i = 0; i < array.length; ++i)
        iHtml += (array[i].dir ==0 ? '&uarr; ' : '&darr; ') + array[i].txt + '<br>';
    target.html(iHtml);
}

function load_div(target, array) {
    var iHtml = '';
    for(var i = 0; i < array.length; ++i)
        iHtml += array[i] + '<br>';
    target.html(iHtml);
}

var sqlObj = null;

//function sql_editor(tblDlg, pTree) {
//    sqlObj = {tables:[],fields:[],sorts:[],conds:[],joins:[]};
//    selected.tables = pTree.tables;
//    $('<div id=sql_edit style="width:100%">'+
//            '<table width=100% height=100% border=0>'+
//                '<tr><td width=50% valign="top">Tables'+
//                        '<div id=sql_edit_tbls class=sql_edit_flds style="width:93%"/>'+
//                    '</td><td width=50% rowspan=2 valign="top">Fields'+
//                        '<div id=sql_edit_flds class=sql_edit_flds style="height:93%"/>'+
//                    '</td></tr>'+
//                '<tr><td width=50% valign="top">Sort Order'+
//                        '<div id=sql_edit_srt class=sql_edit_flds style="width:93%"/>'+
//                    '</td></tr>'+
//                '<tr id="join_inp" hidden><td valign="top" colspan = 2>Table Joins<div id=sql_tbl_join class=sql_edit_flds/></td></tr>'+
//                '<tr><td valign="top" colspan = 2>Conditions<div id=sql_edit_cnd class=sql_edit_flds/></td></tr>'+
//            '</table>'+
//      '</div>'
//    ).appendTo(document.body);
//
//    sqlObj.tables = [];
//    for(var i = 0; i < selected.tables.length; ++i)
//        sqlObj.tables[sqlObj.tables.length] = selected.tables[i];
//
//    load_div($('#sql_edit_tbls'), selected.tables);
//    selected.fields = [];
//    if(pTree.fields.length == 1 && pTree.fields[0] == "*")
//        get_columns(function(data) {
//            var colRows = data.rows;
//            pTree.fields = [];
//            for(var i = 0; i < colRows.length; ++i) {
//                selected.fields[selected.fields.length] = pTree.fields[pTree.fields.length] = colRows[i][0];
//                sqlObj.fields[sqlObj.fields.length] = colRows[i][0];
//            }
//            load_div($('#sql_edit_flds'), selected.fields);
//        });
//    else {
//        for(var i = 0; i < pTree.fields.length; ++i)
//            selected.fields[selected.fields.length] = sqlObj.fields[sqlObj.fields.length] = pTree.fields[i];
//        load_div($('#sql_edit_flds'), selected.fields);
//    }
//    selected.sorts = [];
//    for(var i = 0; i < pTree.sorts.length; ++i)
//        selected.sorts[selected.sorts.length] = sqlObj.sorts[sqlObj.sorts.length] = pTree.sorts[i];
//    load_srt_div($('#sql_edit_srt'), selected.sorts);
//
//    $('#sql_edit_tbls').click(function(){
//        sql_tables();
//        selected.tables = $('#sql_edit_tbls').html().split('<br>');
//        for(var i=0; i<selected.tables.length; ++i)
//            if(selected.tables[i].length == 0) selected.tables.splice(i,1);
//        reload_selection_table();
//    });
//    $('#sql_edit_flds').click(function(){
//        sql_fields();
//        selected.fields = $('#sql_edit_flds').html().split('<br>');
//        for(var i=0; i<selected.fields.length; ++i)
//            if(selected.fields[i].length == 0) selected.fields.splice(i,1);
//        reload_selection_fields();
//    });
//    $('#sql_edit_srt').click(function(){
//        sql_sorts();
//        var sorts = $('#sql_edit_srt').html().split('<br>');
//        for(var i=0; i<sorts.length && sorts[i].length>0; ++i) {
//            var s = sorts[i].split(' ');
//            var sdir = ($.trim(s[0]).charCodeAt(0) == 8593 ? 0 : 1);
//            selected.sorts[i] = {txt: $.trim(s[1]), dir:sdir};
//        }
//        for(var i=0; i<selected.sorts.length; ++i)
//            if(selected.sorts[i].length == 0) selected.sorts.splice(i,1);
//        reload_selection_sorts();
//    });
//
//    $('#sql_edit').dialog({
//        autoOpen: false,
//        height: 400,
//        width: 400,
//        modal: true,
//        position: [100, 100],
//        resizable: false,
//        title: "Query Editor",
//        close: function() {
//            $('#sql_edit').dialog('destroy');
//            $('#sql_edit').remove();
//        },
//        buttons: {
//            "Ok": function() {
//                $('#sql_edit').dialog('close');
//                tblDlg.trigger('requery', sqlObj);
//            },
//            "Cancel": function() {
//                $('#sql_edit').dialog('close');
//            }
//        }
//    });
//    $('#sql_edit').dialog("open");
//}

/*
{
select:["*"],
options:[],
from:["abc","def"],
where: {op:"and", argList:[]},
groupby:[],
having:[],
order_by:[]
}
 */
