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

function disable(container) {
    var divDisable = document.createElement('div');
    divDisable.className = 'ui-dialog-disabled';
    container.append(divDisable);
    return $(divDisable);
}

function enable(divDisable) {
    divDisable.remove();
}

function buildCtxHandler(container, inp) {
    return function (e) {
        e.preventDefault();
        console.log("context menu");

        var mainBody = document.getElementById("main-body");
        var menu = document.createElement("ul");
        menu.className = "context_menu";
        menu.style.top = (e.clientY - 35) + "px";
        menu.style.left = (e.clientX - 10) + "px";
        menu.onmouseleave = function() {
            mainBody.removeChild(menu);
        };
        var menuEntry = document.createElement("li");
        menu.appendChild(menuEntry);
        menuEntry.appendChild(document.createTextNode("Edit"));
        menuEntry.onclick = function() {
            var content = {isFormatted: true, string: inp.val()};
            var isJson = false;
            var title = "Text editor";
            try {
                if(JSON.parse(content.string)) {
                    isJson = true;
                    title = "Json editor";
                    content = content.string;
                }
            } catch (e) {}

            var termOwner = {};
            termOwner._divDisable = disable(container);
            termOwner.enableDialog = function() {
                enable(termOwner._divDisable);
            };
            termOwner.updateErlangCell = function(newValue) {
                inp.val(newValue);
            };

            $('<div>')
                .appendTo(document.body)
                .termEditor({
                    autoOpen: false,
                    title: title,
                    termOwner: termOwner,
                    readOnly: false,
                    container: $("#main-body"),
                    appendTo: "#main-body",
                    term: content,
                    isJson: isJson
                }).termEditor('open');
            mainBody.removeChild(menu);
        };
        mainBody.appendChild(menu);
        console.log("finished :)");
    };
}

export function clear_out_fields(outInputs) {
    outInputs.forEach(function(outParam) {
        outParam.inp.val("");
    });
}

export function result_out_params(data, outInputs) {
    outInputs.forEach(function(outParam) {
        for(var param in data) {
            if(outParam.name == param) {
                outParam.inp.val(data[param]);
            }
        }
    });
}

export function sql_params_dlg(container, qpars, outInputs) {
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

        i.val(param.val)
            .on('focus', inpFocusHandler)
            .on('contextmenu', buildCtxHandler(container, i));
        
        s.find('option[value="'+param.typ+'"]').attr('selected','selected');
        $('<tr/>')
            .append($('<td>' + param.name + '</td>').addClass('fit-content'))
            .append($('<td/>').addClass('fit-content').append(s))
            .append($('<td/>').append(i))
            .appendTo(tab);
    });
}

export function all_out_params(pars) {
    for(let p in pars) {
        if(pars[p].dir === 'in') {
            return false;
        }
    }
    return true;
}
