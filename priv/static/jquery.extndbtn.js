(function( $ ) {
  $.fn.extend_btn = function() {
    $(this)
      .button({
          text: false,
          icons: { primary: $(this).attr("picon"),
                   secondary: $(this).attr("sicon")
          }
      })
      .click(function() {
          $(this).next().toggle();
      })
      .next()
        .children().each(function(index){
                $(this).button({text: false,
                           icons: { primary: $(this).attr("picon") }
            })
        });
      $(this)
          .next()
          .buttonset()
          .css('float', 'top');

      return $(this);
  };
})( jQuery );
