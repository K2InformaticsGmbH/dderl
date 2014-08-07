describe("api/ng.directive:ngInclude", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:ngInclude");
    });

    var templateSelect = element(by.model('template'));
    var includeElem = element(by.css('.doc-example-live [ng-include]'));
    
    it('should load template1.html', function() {
      expect(includeElem.getText()).toMatch(/Content of template1.html/);
    });
    
    it('should load template2.html', function() {
      if (browser.params.browser == 'firefox') {
        // Firefox can't handle using selects
        // See https://github.com/angular/protractor/issues/480
        return;
      }
      templateSelect.click();
      templateSelect.element.all(by.css('option')).get(2).click();
      expect(includeElem.getText()).toMatch(/Content of template2.html/);
    });
    
    it('should change to blank', function() {
      if (browser.params.browser == 'firefox') {
        // Firefox can't handle using selects
        return;
      }
      templateSelect.click();
      templateSelect.element.all(by.css('option')).get(0).click();
      expect(includeElem.isPresent()).toBe(false);
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:ngInclude");
    });
    var templateSelect = element(by.model('template'));
    var includeElem = element(by.css('.doc-example-live [ng-include]'));
    
    it('should load template1.html', function() {
      expect(includeElem.getText()).toMatch(/Content of template1.html/);
    });
    
    it('should load template2.html', function() {
      if (browser.params.browser == 'firefox') {
        // Firefox can't handle using selects
        // See https://github.com/angular/protractor/issues/480
        return;
      }
      templateSelect.click();
      templateSelect.element.all(by.css('option')).get(2).click();
      expect(includeElem.getText()).toMatch(/Content of template2.html/);
    });
    
    it('should change to blank', function() {
      if (browser.params.browser == 'firefox') {
        // Firefox can't handle using selects
        return;
      }
      templateSelect.click();
      templateSelect.element.all(by.css('option')).get(0).click();
      expect(includeElem.isPresent()).toBe(false);
    });

  });
});
