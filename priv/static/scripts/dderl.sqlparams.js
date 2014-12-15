function sql_params_dlg(container, qpars)
{
    container.empty();

    tab = $('<table/>')
        .addClass('params')
        .append($('<tr><th>Param</th>'+
                    '<th>Type</th>'+
                    '<th>Value</th>'+
                    '</tr>'))
        .appendTo(container);
    
    sel = $('<select/>')
        .css('font-family', 'inherit')
        .css('font-weight', 'inherit')
        .css('font-size', 'inherit');

    qpars.types.forEach(function(v,i,a) {
        sel.append($('<option/>', {value:v}).html(v));
    });

    inp = $('<input type="text"/>')
        .css('font-family', 'inherit')
        .css('font-weight', 'inherit')
        .css('font-size', 'inherit');
    
    for (p in qpars.pars) {
        param = qpars.pars[p];
        s = sel.clone();
        i = inp.clone();
        (function(_s,_i,par) {
            _s.change(function() {
                setTimeout (function() {
                    par.typ = _s.val();
                }, 1)
            });
            _i.change(function() {
                setTimeout (function() {
                    par.val = _i.val();
                }, 1)
            });
        })(s,i,param);

        s.find('option[value="'+param.typ+'"]').attr('selected','selected');
        tab.append($('<tr/>').append($('<td>'+p+'</td>'))
                        .append($('<td/>').append(s))
                        .append($('<td/>').append(i.val(param.val))));
    }
}
