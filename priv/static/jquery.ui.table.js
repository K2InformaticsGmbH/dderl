(function($) {
$.widget("ui.table", {

options: {
  location: "bottom",
  color: "#fff",
  backgroundColor: "#000",
  rows: [],
  columns: []
},

_create: function() {

    var self = this,
        o = self.options,
        el = self.element,
        tbl_id = el.attr('id');


    var row;
    if(o.rows.length > 0) {
        var div = $('<div id="d_'+tbl_id+'_container" class="tableContainer"/>').appendTo(el),
            tbl = $('<table id="tr_'+tbl_id+'" border="0" cellpadding="0" cellspacing="0" class="scrollTable"/>').appendTo(div);

        var tHdr = $('<thead class="fixedHeader"/>').appendTo(tbl);
        row = $('<tr/>').appendTo(tHdr);
        $('<th class="tbl-header">Hi</th>').appendTo(row);
        for(var c = 0; c < o.cols.length; ++c) {
            $('<th id="'+tbl_id+'_c'+c+'" class="tbl-header">'+o.cols[c]+'</th>')
                .click(function() {
                    $('.col-sel').removeClass('col-sel');
                    $('td[id~="'+$(this).attr('id')+'"]').addClass('col-sel');
                    $('th[id~="'+$(this).attr('id')+'"]').addClass('col-sel');
                })
                .appendTo(row);
        }
        var tBody = $('<tbody class="scrollContent">').appendTo(tbl);
        for(var r = 0; r < o.rows.length; ++r) {
            if(o.rows[r].length == o.cols.length) {
                row = $('<tr/>').appendTo(tBody);
                $('<td id="'+tbl_id+'_r'+r+'" class="tbl-header">'+r+'</td>')
                    .click(function() {
                        $('.col-sel').removeClass('col-sel');
                        var dtid = $(this).attr('id').replace(/_c/i,"_r");
                        $('td[id~="'+dtid+'"]').addClass('col-sel');
                    })
                    .appendTo(row);
                for(var c = 0; c < o.rows[r].length; ++c) {
                    $('<td id="'+tbl_id+'_c'+c+' '+tbl_id+'_r'+r+'">&nbsp;'+o.rows[r][c]+'&nbsp;</td>')
                        .click(function() { alert("Cell"); })
                        .appendTo(row);
                }
            }
        }
    }
},

destroy: function() {
    this.element.next().remove();

    $(window).unbind("resize");
},

_setOption: function(option, value) {
    $.Widget.prototype._setOption.apply( this, arguments );

    var el = this.element,
        cap = el.next(),
        capHeight = cap.outerHeight()-parseInt(cap.css("paddingTop"))+parseInt(cap.css("paddingBottom"));

    switch (option) {
        case "location":
            (value === "top") ? cap.css("top", el.offset().top) : cap.css("top", el.offset().top + el.height() - capHeight);
            break;
        case "color":
            el.next().css("color", value);
            break;
        case "backgroundColor":
            el.next().css("backgroundColor", value);
            break;
        case "rows":
            //el.next().css("rows", value);
            break;
        case "cols":
            //el.next().css("cols", value);
            break;
    }
}

});
})(jQuery);
