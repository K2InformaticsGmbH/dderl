describe("api/ng.directive:ngInit", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:ngInit");
    });

    it('should alias index positions', function() {
      var elements = element.all(by.css('.example-init'));
      expect(elements.get(0).getText()).toBe('list[ 0 ][ 0 ] = a;');
      expect(elements.get(1).getText()).toBe('list[ 0 ][ 1 ] = b;');
      expect(elements.get(2).getText()).toBe('list[ 1 ][ 0 ] = c;');
      expect(elements.get(3).getText()).toBe('list[ 1 ][ 1 ] = d;');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:ngInit");
    });
    it('should alias index positions', function() {
      var elements = element.all(by.css('.example-init'));
      expect(elements.get(0).getText()).toBe('list[ 0 ][ 0 ] = a;');
      expect(elements.get(1).getText()).toBe('list[ 0 ][ 1 ] = b;');
      expect(elements.get(2).getText()).toBe('list[ 1 ][ 0 ] = c;');
      expect(elements.get(3).getText()).toBe('list[ 1 ][ 1 ] = d;');
    });

  });
});
