var parameters = Session.Property("CustomActionData").split("|");
var path = parameters[0] + 'bin\\dderl.cmd';
var action = parameters[1];
var WshShell = new ActiveXObject("WScript.Shell");

if (action == "install") {
    WshShell.Exec(path + ' install');
}
else if (action == "commit") {
    WshShell.Exec(path + ' start');
}
else if (action == "uninstall") {
    var sharedFso = new ActiveXObject("Scripting.FileSystemObject");
    if (sharedFso.FileExists(path)) {
        WshShell.Exec(path + ' stop');
        WshShell.Exec(path + ' uninstall');
    } else {
        var msiMessageTypeUser = 0x03000000;
        var msiMessageTypeYesNo = 4;
        var msiMessageTypeDefault1 = 0x000;

        var options = msiMessageTypeUser + msiMessageTypeYesNo + msiMessageTypeDefault1;
        var objRecord = Session.Installer.CreateRecord(1);
        objRecord.StringData(0) = "[1]";
        objRecord.StringData(1) = "Datei nicht gefunden '" + path
                                    + "' Deinstallation möglicherweise nicht erfolgreich";

        var response = Session.Message(options, objRecord);
    }
}
