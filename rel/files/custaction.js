var msiMessageTypeUser = 0x03000000;
var msiMessageTypeYesNo = 4;
var msiMessageTypeDefault1 = 0x000;

var options = msiMessageTypeUser
        + msiMessageTypeYesNo
        + msiMessageTypeDefault1;

var path = Session.Property("CustomActionData");

var objRecord = Session.Installer.CreateRecord(1);
objRecord.StringData(0) = "[1]";
objRecord.StringData(1) = path;

var shell = new ActiveXObject("WScript.Shell");

var response = Session.Message(options, objRecord);