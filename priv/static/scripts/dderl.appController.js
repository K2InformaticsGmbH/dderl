angular.module('dderlAngularApp', ['ngRoute', 'ui.ace'], function($routeProvider) {
    $routeProvider.when('/showAngular', {
        templateUrl: 'templates/properties_panel.html',
        controller: 'dderlAppController'
    });

    $routeProvider.when('/showBox', {
        templateUrl: 'templates/sqlbox.html',
        controller: 'sqlboxController'
    });
    
    $routeProvider.when('/showAceEditor', {
        templateUrl: 'templates/ace_editor.html',
        controller: 'aceController'
    });
});

function dderlAppController($scope) {
    $scope.columns = [];
    $scope.visible = true;

    // This should toggle to show or not the 
    $scope.toggleVisible = function() {
        $scope.visible = !$scope.visible;
    }
}

// Helpers to apply functions if the scope is available.
function get_view_scope() {
    return angular.element(document.getElementById("angular-view")).scope();
}

function scope_apply(theAwesomeFun) {
    var scope = get_view_scope();
    if(scope) {
        scope.$apply(theAwesomeFun(scope));
    }
}

// External functions.
function toggle_visible() {
    scope_apply(function(escope) {
        escope.toggleVisible();
    });
}

function set_panel_columns(columns) {
    scope_apply(function(escope) {
        escope.columns = columns.slice(1);
    });
}

function is_angular_boxing() {
    var scope = get_view_scope();
    if(scope) {
        return scope.angular_boxing;
    }
    return false;
}

function is_ace_editor() {
    var scope = get_view_scope();
    if(scope) {
        return scope.ace_editor;
    }
    return false;
} 