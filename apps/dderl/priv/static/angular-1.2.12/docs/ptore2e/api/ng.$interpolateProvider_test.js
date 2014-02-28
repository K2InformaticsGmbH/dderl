describe("api/ng.$interpolateProvider", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.$interpolateProvider");
    });

    it('should interpolate binding with custom symbols', function() {
      expect(element(by.binding('demo.label')).getText()).toBe('This binding is brought you by // interpolation symbols.');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.$interpolateProvider");
    });
    it('should interpolate binding with custom symbols', function() {
      expect(element(by.binding('demo.label')).getText()).toBe('This binding is brought you by // interpolation symbols.');
    });

  });
});
