import $ from 'jquery';

function addUpdParamHandlers(el, par, prop) {
    el.change(function() {
        setTimeout(function() {
            par[prop] = el.val();
        }, 1);
    });
}
function inpFocusHandler() {
    var $this = $(this);

    $this.one('mouseup.mouseupSelect', function() {
        $this.select();
        return false;
    });
        
    $this.one('mousedown', function() {
        $this.off('mouseup.mouseupSelect');
    }).select();
}

export function sql_params_dlg(container, qpars) {
    container.empty();

    var tab = $('<table/>')
        .addClass('params')
        .appendTo(container);
    
    var sel = $('<select/>')
        .css('font-family', 'inherit')
        .css('font-weight', 'inherit')
        .css('font-size', 'inherit');

    qpars.types.forEach(function(v) {
        sel.append($('<option/>', {value:v}).html(v));
    });

    var inp = $('<input type="text"/>')
        .addClass('param')
        .css('font-family', 'inherit')
        .css('font-weight', 'inherit')
        .css('font-size', 'inherit');

    for (let p in qpars.pars) {
        let param = qpars.pars[p];
        let s = sel.clone();
        let i = inp.clone();

        addUpdParamHandlers(s, param, "typ");
        addUpdParamHandlers(i, param, "val");

        s.find('option[value="'+param.typ+'"]').attr('selected','selected');
        $('<tr/>')
            .append($('<td>' + p + '</td>').addClass('fit-content'))
            .append($('<td/>').addClass('fit-content').append(s))
            .append($('<td/>').append(i.on('focus', inpFocusHandler).val(param.val)))
            .appendTo(tab);
    }
}
