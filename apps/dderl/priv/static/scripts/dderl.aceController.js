ace.require("ace/ext/language_tools");

function aceController($scope) {
    $scope.boxEditors = [];

    // Model flag to indicate to the externall functions
    // the module in use for the view.
    $scope.ace_editor = true;

    $scope.getEditorScope = function(uniqueId) {
        for(var i = 0; i < $scope.boxEditors.length; ++i) {
            if($scope.boxEditors[i].id === uniqueId) {
                return $scope.boxEditors[i];
            }
        }
    };
}

function create_ace_editor(ownerDlg) {
    var uniqueId = "boxdiv_" + new Date().getTime().toString();
    scope_apply(function(collectionScope) {
        var aceconfig = {
            useWrapMode : true,
            showGutter: true,
            theme:'chrome',
            mode: 'mysql',
            onLoad: function(editor) {
                editor.setOptions({
                    enableBasicAutocompletion: true,
                    enableSnippets: true
                });

                collectionScope.getEditorScope(uniqueId).editorRedraw = function() {
                    editor.resize();
                    editor.renderer.updateFull();
                }
            },
            onChange: function(e) {
                // TODO: Probably we want to set the cmd back....
            }
        }
        collectionScope.boxEditors.push({id: uniqueId, content: "", aceconfig: aceconfig, ownerDlg: ownerDlg});
    });

    scope_apply(function(collectionScope) {
        for(var i = 0; i < collectionScope.boxEditors.length; ++i) {
            if(collectionScope.boxEditors[i].id !== uniqueId) {
                collectionScope.boxEditors[i].ownerDlg.getEditor();
            }
        }
    });

    return uniqueId;
}

function set_sql_content(uniqueId, content) {
    scope_apply(function(collectionScope) {
        var editorScope = collectionScope.getEditorScope(uniqueId);
        editorScope.content = content;
        editorScope.editorRedraw();
    });
}

function resize_ace_editor(uniqueId) {
    scope_apply(function (collectionScope) {
        collectionScope.getEditorScope(uniqueId).editorRedraw();
    });
}
