describe("api/ng.directive:input.text", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:input.text");
    });

    var text = element(by.binding('text'));
    var valid = element(by.binding('myForm.input.$valid'));
    var input = element(by.model('text'));
    
    it('should initialize to model', function() {
      expect(text.getText()).toContain('guest');
      expect(valid.getText()).toContain('true');
    });
    
    it('should be invalid if empty', function() {
      input.clear();
      input.sendKeys('');
    
      expect(text.getText()).toEqual('text =');
      expect(valid.getText()).toContain('false');
    });
    
    it('should be invalid if multi word', function() {
      input.clear();
      input.sendKeys('hello world');
    
      expect(valid.getText()).toContain('false');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:input.text");
    });
    var text = element(by.binding('text'));
    var valid = element(by.binding('myForm.input.$valid'));
    var input = element(by.model('text'));
    
    it('should initialize to model', function() {
      expect(text.getText()).toContain('guest');
      expect(valid.getText()).toContain('true');
    });
    
    it('should be invalid if empty', function() {
      input.clear();
      input.sendKeys('');
    
      expect(text.getText()).toEqual('text =');
      expect(valid.getText()).toContain('false');
    });
    
    it('should be invalid if multi word', function() {
      input.clear();
      input.sendKeys('hello world');
    
      expect(valid.getText()).toContain('false');
    });

  });
});
