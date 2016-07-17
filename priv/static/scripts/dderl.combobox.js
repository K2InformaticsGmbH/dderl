import $ from 'jquery';
import "jquery-ui/ui/autocomplete";
import "jquery-ui/ui/tooltip";

//From http://jqueryui.com/autocomplete/#combobox
$.widget( "custom.combobox", {
    _create: function() {
        this.wrapper = $( "<span>" )
            .addClass( "custom-combobox" )
            .insertAfter( this.element );
        this.element.hide();
        this._createAutocomplete();
        this._createShowAllButton();
    },
    _createAutocomplete: function() {
        var selected = this.element.children( ":selected" ),
        value = selected.val() ? selected.text() : "";
        this.input = $( "<input>" )
            .appendTo( this.wrapper )
            .val( value )
            .attr( "title", "" )
            .attr( "id", this.element.attr("id") + "-input")
            .addClass("ui-widget ui-widget-content ui-corner-left")
            .autocomplete({
                delay: 0,
                minLength: 0,
                source: $.proxy( this, "_source" ),
                appendTo: this.wrapper
            })
            .tooltip({
                tooltipClass: "ui-state-highlight"
            });
        this.input.addClass("custom-combobox-input");
        this._on( this.input, {
            autocompleteselect: function( event, ui ) {
                ui.item.option.selected = true;
                this._trigger( "select", event, {
                    item: ui.item.option
                });
                this.element.trigger("change");
            },

            // ui here is always empty.
            autocompletechange: function() {

                // Search for a match (case-sensitive)
                var self = this;
                var value = self.input.val();

                self.element.children( "option" ).each(function() {
                    if ( $( this ).text() === value ) {
                        this.selected = true;
                        self.element.trigger("change");
                        return false;
                    }
                });
            }
        });
    },
    _createShowAllButton: function() {
        var input = this.input,
        wasOpen = false;
        $( "<a>" )
            .attr( "tabIndex", -1 )
            .attr( "title", "Show All Items" )
            .tooltip()
            .appendTo( this.wrapper )
            .button({
                icons: {
                    primary: "ui-icon-triangle-1-s"
                },
                text: false
            })
            .removeClass( "ui-corner-all" )
            .addClass( "custom-combobox-toggle ui-corner-right" )
            .mousedown(function() {
                wasOpen = input.autocomplete( "widget" ).is( ":visible" );
            })
            .click(function() {
                input.focus();
                // Close if already visible
                if ( wasOpen ) {
                    return;
                }
                // Pass empty string as value to search for, displaying all results
                input.autocomplete( "search", "" );
            });
    },
    _source: function( request, response ) {
        var matcher = new RegExp( $.ui.autocomplete.escapeRegex(request.term), "i" );
        response( this.element.children( "option" ).map(function() {
            var text = $( this ).text();
            if ( this.value && ( !request.term || matcher.test(text) ) )
                return {
                    label: text,
                    value: text,
                    option: this
                };
        }) );
    },
    _destroy: function() {
        this.wrapper.remove();
        this.element.show();
    }
});
