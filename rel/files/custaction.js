var MsgConfig =
{
    Type: {
        FatalExit: 0x00000000,      // Premature termination, possibly fatal out of memory
        Error: 0x01000000,          // Formatted error message, [1] is message number in Error table.
        Warning: 0x02000000,        // Formatted warning message, [1] is message number in Error table.
        User: 0x03000000,           // User request message, [1] is message number in Error table.
        Info: 0x04000000,           // Informative message for log, not to be displayed.
        FilesInUse: 0x05000000,     // List of files in use that need to be replaced.
        ResolveSource: 0x06000000,  // Request to determine a valid source location.
        OutOfDiskSpace: 0x07000000, // Insufficient disk space message.
        ActionStart: 0x08000000,    // Start of action, [1] action name, [2] description, [3] template for ACTIONDATA messages.
        ActionData: 0x09000000,     // Action data. Record fields correspond to the template of ACTIONSTART message.
        Progress: 0x0A000000,       // Progress bar information. See the description of record fields below.
        CommonData: 0x0B000000      // To enable the Cancel button set [1] to 2 and [2] to 1. To disable the Cancel button set [1] to 2 and [2] to 0
    },
    Button: {
        Ok: 0x00000000,
        AbortRetryIgnore: 0x00000002,
        CancelRetryContinue: 0x00000006,
        Help: 0x00004000,
        OkCancel: 0x00000001,
        RetryCancel: 0x00000005,
        YesNo: 0x00000004,
        YesNoCancel: 0x00000003
    },
    DefaultBtn: {
        First: 0x00000000,
        Second: 0x00000100,
        Third: 0x00000200,
        Fourth: 0x00000300
    },
    Icon: {
        Exclamation: 0x00000030,    // An exclamation-point icon appears in the message box.
        Warning: 0x00000030,        // An exclamation-point icon appears in the message box.
        Information: 0x00000040,    // An icon consisting of a lowercase letter i in a circle appears in the message box.
        Asterix: 0x00000040,        // An icon consisting of a lowercase letter i in a circle appears in the message box.
        Question: 0x00000020,       // A question-mark icon appears in the message box. The question-mark message icon is no longer recommended because it does not clearly represent a specific type of message and because the phrasing of a message as a question could apply to any message type. In addition, users can confuse the message symbol question mark with Help information. Therefore, do not use this question mark message symbol in your message boxes. The system continues to support its inclusion only for backward compatibility.
        Stop: 0x00000010,           // A stop-sign icon appears in the message box.
        Error: 0x00000010,          // A stop-sign icon appears in the message box.
        Hand: 0x00000010            // A stop-sign icon appears in the message box.
    }
};

var parameters = Session.Property("CustomActionData").split("|");
var installPath = parameters[0];
var dderlCmdPath = installPath + 'bin\\dderl.cmd';
var action = parameters[1];
var WshShell = new ActiveXObject("WScript.Shell");
var FileSys = new ActiveXObject("Scripting.FileSystemObject");

if (action == "install") {
}
else if (action == "commit") {
    WshShell.Exec(dderlCmdPath + ' install');
    MsgBox("Service installed", MsgConfig.Type.User, MsgConfig.Button.Ok, MsgConfig.DefaultBtn.First, MsgConfig.Icon.Exclamation);
    WshShell.Exec(dderlCmdPath + ' start');
}
else if (action == "uninstall") {
    if (FileSys.FileExists(dderlCmdPath)) {
        WshShell.Exec(dderlCmdPath + ' stop');
        WshShell.Exec(dderlCmdPath + ' uninstall');
    } else {
        MsgBox("Datei nicht gefunden '" + path + "' Deinstallation möglicherweise nicht erfolgreich",
            MsgConfig.Type.User, MsgConfig.Button.Ok, MsgConfig.DefaultBtn.First, MsgConfig.Icon.Exclamation);
    }
}

function MsgBox(message, type, buttons, defaultbutton, icon)
{
    var options = type + buttons + defaultbutton + icon;
    var objRecord = Session.Installer.CreateRecord(1);
    objRecord.StringData(0) = "[1]";
    objRecord.StringData(1) = message;

    return Session.Message(options, objRecord);
}
