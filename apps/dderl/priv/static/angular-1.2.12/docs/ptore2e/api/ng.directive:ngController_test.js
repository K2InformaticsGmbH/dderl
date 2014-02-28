describe("api/ng.directive:ngController", function() {
  describe("angular+jqLite", function() {
    beforeEach(function() {
      browser.get("index-nocache.html#!/api/ng.directive:ngController");
    });

      it('should check controller as', function() {
        var container = element(by.id('ctrl-as-exmpl'));
    
        expect(container.findElement(by.model('settings.name'))
            .getAttribute('value')).toBe('John Smith');
    
        var firstRepeat =
            container.findElement(by.repeater('contact in settings.contacts').row(0));
        var secondRepeat =
            container.findElement(by.repeater('contact in settings.contacts').row(1));
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('408 555 1212');
        expect(secondRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('john.smith@example.org');
    
        firstRepeat.findElement(by.linkText('clear')).click()
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('');
    
        container.findElement(by.linkText('add')).click();
    
        expect(container.findElement(by.repeater('contact in settings.contacts').row(2))
            .findElement(by.model('contact.value'))
            .getAttribute('value'))
            .toBe('yourname@example.org');
      });

      it('should check controller', function() {
        var container = element(by.id('ctrl-exmpl'));
    
        expect(container.findElement(by.model('name'))
            .getAttribute('value')).toBe('John Smith');
    
        var firstRepeat =
            container.findElement(by.repeater('contact in contacts').row(0));
        var secondRepeat =
            container.findElement(by.repeater('contact in contacts').row(1));
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('408 555 1212');
        expect(secondRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('john.smith@example.org');
    
        firstRepeat.findElement(by.linkText('clear')).click()
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('');
    
        container.findElement(by.linkText('add')).click();
    
        expect(container.findElement(by.repeater('contact in contacts').row(2))
            .findElement(by.model('contact.value'))
            .getAttribute('value'))
            .toBe('yourname@example.org');
      });

  });
  describe("angular+jQuery", function() {
    beforeEach(function() {
      browser.get("index-jq-nocache.html#!/api/ng.directive:ngController");
    });
      it('should check controller as', function() {
        var container = element(by.id('ctrl-as-exmpl'));
    
        expect(container.findElement(by.model('settings.name'))
            .getAttribute('value')).toBe('John Smith');
    
        var firstRepeat =
            container.findElement(by.repeater('contact in settings.contacts').row(0));
        var secondRepeat =
            container.findElement(by.repeater('contact in settings.contacts').row(1));
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('408 555 1212');
        expect(secondRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('john.smith@example.org');
    
        firstRepeat.findElement(by.linkText('clear')).click()
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('');
    
        container.findElement(by.linkText('add')).click();
    
        expect(container.findElement(by.repeater('contact in settings.contacts').row(2))
            .findElement(by.model('contact.value'))
            .getAttribute('value'))
            .toBe('yourname@example.org');
      });

      it('should check controller', function() {
        var container = element(by.id('ctrl-exmpl'));
    
        expect(container.findElement(by.model('name'))
            .getAttribute('value')).toBe('John Smith');
    
        var firstRepeat =
            container.findElement(by.repeater('contact in contacts').row(0));
        var secondRepeat =
            container.findElement(by.repeater('contact in contacts').row(1));
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('408 555 1212');
        expect(secondRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('john.smith@example.org');
    
        firstRepeat.findElement(by.linkText('clear')).click()
    
        expect(firstRepeat.findElement(by.model('contact.value')).getAttribute('value'))
            .toBe('');
    
        container.findElement(by.linkText('add')).click();
    
        expect(container.findElement(by.repeater('contact in contacts').row(2))
            .findElement(by.model('contact.value'))
            .getAttribute('value'))
            .toBe('yourname@example.org');
      });

  });
});
