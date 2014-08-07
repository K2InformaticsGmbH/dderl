describe("api/ng.directive:select", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:select");
    });

    it('should check ng-options', function() {
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('red');
      element.all(by.select('color')).first().click();
      element.all(by.css('select[ng-model="color"] option')).first().click();
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('black');
      element(by.css('.nullable select[ng-model="color"]')).click();
      element.all(by.css('.nullable select[ng-model="color"] option')).first().click();
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('null');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:select");
    });
    it('should check ng-options', function() {
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('red');
      element.all(by.select('color')).first().click();
      element.all(by.css('select[ng-model="color"] option')).first().click();
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('black');
      element(by.css('.nullable select[ng-model="color"]')).click();
      element.all(by.css('.nullable select[ng-model="color"] option')).first().click();
      expect(element(by.binding('{selected_color:color}')).getText()).toMatch('null');
    });

  });
});
