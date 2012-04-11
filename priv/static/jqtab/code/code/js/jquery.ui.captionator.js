(function($) {

    $.widget("ui.captionator", {
		options: {
			location: "bottom",
			color: "#fff",
			backgroundColor: "#000"
		},
				
		_create: function() {
			
			var self = this,
				o = self.options,
				el = self.element,
				cap = $("<span></span>").text(el.attr("alt")).addClass("ui-widget ui-caption").css({
					backgroundColor: o.backgroundColor,
					color: o.color,
					width: el.width()
				}).insertAfter(el),
				capWidth = el.width() - parseInt(cap.css("paddingLeft")) - parseInt(cap.css("paddingRight")),
				capHeight = cap.outerHeight() - parseInt(cap.css("paddingTop")) + parseInt(cap.css("paddingBottom"));
				
			cap.css({
				width: capWidth,
				top: (o.location === "top") ? el.offset().top : el.offset().top + el.height() - capHeight,
				left: el.offset().left,
				display: "block"
			});
			
			self._trigger("added", null, cap);
			
			$(window).resize(function(){
				cap.css({
					top: (o.location === "top") ? el.offset().top : el.offset().top + el.height() - capHeight,
					left: el.offset().left
				});
			});
		},
				
		destroy: function() {			
			this.element.next().remove();
			
			$(window).unbind("resize");
		},
		
		_setOption: function(option, value) {
			$.Widget.prototype._setOption.apply( this, arguments );
			
			var el = this.element,
				cap = el.next(),
				capHeight = cap.outerHeight() - parseInt(cap.css("paddingTop")) + parseInt(cap.css("paddingBottom"));
			
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
			}
		}
	});
})(jQuery);