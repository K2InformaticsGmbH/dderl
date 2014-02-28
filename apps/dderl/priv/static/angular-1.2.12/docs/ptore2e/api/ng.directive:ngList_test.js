describe("api/ng.directive:ngList", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:ngList");
    });

    var listInput = element(by.model('names'));
    var names = element(by.binding('{{names}}'));
    var valid = element(by.binding('myForm.namesInput.$valid'));
    var error = element(by.css('span.error'));
    
    it('should initialize to model', function() {
      expect(names.getText()).toContain('["igor","misko","vojta"]');
      expect(valid.getText()).toContain('true');
      expect(error.getCssValue('display')).toBe('none');
    });
    
    it('should be invalid if empty', function() {
      listInput.clear();
      listInput.sendKeys('');
    
      expect(names.getText()).toContain('');
      expect(valid.getText()).toContain('false');
      expect(error.getCssValue('display')).not.toBe('none');        });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:ngList");
    });
    var listInput = element(by.model('names'));
    var names = element(by.binding('{{names}}'));
    var valid = element(by.binding('myForm.namesInput.$valid'));
    var error = element(by.css('span.error'));
    
    it('should initialize to model', function() {
      expect(names.getText()).toContain('["igor","misko","vojta"]');
      expect(valid.getText()).toContain('true');
      expect(error.getCssValue('display')).toBe('none');
    });
    
    it('should be invalid if empty', function() {
      listInput.clear();
      listInput.sendKeys('');
    
      expect(names.getText()).toContain('');
      expect(valid.getText()).toContain('false');
      expect(error.getCssValue('display')).not.toBe('none');        });

  });
});
