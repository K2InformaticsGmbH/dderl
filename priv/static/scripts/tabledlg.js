(function( $ ) {
  $.widget( "dderl.table", $.ui.dialog, {
 
    // These options will be used as defaults
    options: { 
      clear: null
    },
 
    // Set up the widget
    _create: function() {
        var self = this;
        this.element.dialog(this.options);
    },
 
    // Use the _setOption method to respond to changes to options
    _setOption: function( key, value ) {
      switch( key ) {
        case "clear":
          // handle changes to clear option
          break;
      }
 
      // In jQuery UI 1.9 and above, you use the _super method instead
      this._super( "_setOption", key, value );
    },
 
    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
    }
  });
}( jQuery ) );

