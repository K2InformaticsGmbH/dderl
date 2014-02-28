function aceController($scope) {
    $scope.content = "";

    // Model flag to indicate to the externall functions
    // the module in use for the view.
    $scope.ace_editor = true;

    $scope.aceconfig = {
        useWrapMode : true,
        showGutter: true,
        theme:'chrome',
        mode: 'mysql',
        onLoad: function(editor) {
            editor.setOptions({
                enableBasicAutocompletion: true,
                enableSnippets: true
            });

            $scope.editorRedraw = function() {
                editor.resize();
                editor.renderer.updateFull();
            }
        },
        onChange: function(e) {
            // TODO: Probably we want to set the cmd back....
        }
    }
}

ace.require("ace/ext/language_tools");

// TODO: This could be replaced by a single function to encapsulate boxing.
function set_sql_content(content) {
    scope_apply(function(editorScope) {
        editorScope.content = content;
        editorScope.editorRedraw();
    });
}

function resize_ace_editor() {
    scope_apply(function (editorScope) {
        editorScope.editorRedraw();
    });
}