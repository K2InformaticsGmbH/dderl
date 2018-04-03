import $ from "jquery";

var selectedTable = null;

export function select(table) {
    if(selectable(selectedTable)) {
        selectedTable.hideSelection();
    }

    if(selectable(table)) {
        table.enableSelection();
        selectedTable = table;
    }
}

export const hiddenSelectionClass = "slick-selection-hidden";

function selectable(obj) {
    return obj && $.isFunction(obj.hideSelection) && $.isFunction(obj.enableSelection);
}
