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

var outInputs = [];
export function clear_out_fields() {
    outInputs.forEach(function(outParam) {
        outParam.inp.val("");
    });
}

export function result_out_params(data) {
    outInputs.forEach(function(outParam) {
        for(var param in data) {
            if(outParam.name == param) {
                outParam.inp.val(data[param]);
            }
        }
    });
}

export function sql_params_dlg(container, qpars) {
    outInputs = [];    
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

    var params = [];
    for (let p in qpars.pars) {
        let param = qpars.pars[p];
        param.name = p;
        params.push(param);
    }

    console.log("the params", params);
    params.sort(function(a, b) {
        if(a.dir === b.dir) {
            return a.name.localeCompare(b.name);
        }
        return a.dir.localeCompare(b.dir);
    });

    params.forEach(function(param) {
        let s = sel.clone();
        let i = inp.clone();

        addUpdParamHandlers(s, param, "typ");
        addUpdParamHandlers(i, param, "val");

        if(param.dir === "out") { outInputs.push({name: param.name, inp: i}); }

        s.find('option[value="'+param.typ+'"]').attr('selected','selected');
        $('<tr/>')
            .append($('<td>' + param.name + '</td>').addClass('fit-content'))
            .append($('<td/>').addClass('fit-content').append(s))
            .append($('<td/>').append(i.on('focus', inpFocusHandler).val(param.val)))
            .appendTo(tab);
    });
}
