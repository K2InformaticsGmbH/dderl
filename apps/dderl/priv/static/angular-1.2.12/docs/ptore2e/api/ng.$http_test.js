describe("api/ng.$http", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.$http");
    });

    var status = element(by.binding('status'));
    var data = element(by.binding('data'));
    var fetchBtn = element(by.id('fetchbtn'));
    var sampleGetBtn = element(by.id('samplegetbtn'));
    var sampleJsonpBtn = element(by.id('samplejsonpbtn'));
    var invalidJsonpBtn = element(by.id('invalidjsonpbtn'));
    
    it('should make an xhr GET request', function() {
      sampleGetBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('200');
      expect(data.getText()).toMatch(/Hello, \$http!/)
    });
    
    it('should make a JSONP request to angularjs.org', function() {
      sampleJsonpBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('200');
      expect(data.getText()).toMatch(/Super Hero!/);
    });
    
    it('should make JSONP request to invalid URL and invoke the error handler',
        function() {
      invalidJsonpBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('0');
      expect(data.getText()).toMatch('Request failed');
    });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.$http");
    });
    var status = element(by.binding('status'));
    var data = element(by.binding('data'));
    var fetchBtn = element(by.id('fetchbtn'));
    var sampleGetBtn = element(by.id('samplegetbtn'));
    var sampleJsonpBtn = element(by.id('samplejsonpbtn'));
    var invalidJsonpBtn = element(by.id('invalidjsonpbtn'));
    
    it('should make an xhr GET request', function() {
      sampleGetBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('200');
      expect(data.getText()).toMatch(/Hello, \$http!/)
    });
    
    it('should make a JSONP request to angularjs.org', function() {
      sampleJsonpBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('200');
      expect(data.getText()).toMatch(/Super Hero!/);
    });
    
    it('should make JSONP request to invalid URL and invoke the error handler',
        function() {
      invalidJsonpBtn.click();
      fetchBtn.click();
      expect(status.getText()).toMatch('0');
      expect(data.getText()).toMatch('Request failed');
    });

  });
});
