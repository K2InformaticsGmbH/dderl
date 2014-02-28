describe("api/ng.directive:ngTransclude", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:ngTransclude");
    });

    it('should have transcluded', function() {
      var titleElement = element(by.model('title'));
      titleElement.clear();
      titleElement.sendKeys('TITLE');
      var textElement = element(by.model('text'));
      textElement.clear();
      textElement.sendKeys('TEXT');
      expect(element(by.binding('title')).getText()).toEqual('TITLE');
      expect(element(by.binding('text')).getText()).toEqual('TEXT');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:ngTransclude");
    });
    it('should have transcluded', function() {
      var titleElement = element(by.model('title'));
      titleElement.clear();
      titleElement.sendKeys('TITLE');
      var textElement = element(by.model('text'));
      textElement.clear();
      textElement.sendKeys('TEXT');
      expect(element(by.binding('title')).getText()).toEqual('TITLE');
      expect(element(by.binding('text')).getText()).toEqual('TEXT');
    });

  });
});
