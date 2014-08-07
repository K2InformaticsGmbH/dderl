function sqlboxController($scope) {
    $scope.children = [];
    // Model flag to indicate to the externall functions
    // the module in use for the view.
    $scope.angular_boxing = true;

    $scope.getalltxt = function(box) {
        var alltxt = box.name;
        for(var i = 0; i < box.children.length; ++i) {
            alltxt += (" " + $scope.getalltxt(box.children[i]));
        }
        return alltxt;
    }

    $scope.updatealltxt = function(box) {
        box.name = box.alltxt;
        box.children = []
    }
}

function set_boxing(box) {
    scope_apply(function(boxScope) {
        box.alltxt = boxScope.getalltxt(box);
        boxScope.children = [box];
    });
}
