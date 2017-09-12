// import $ from 'jquery';
import {dderlState} from './dderl';

var receivers = {};

export function open_websocket() {
    if(!("WebSocket" in window)){
        dderlState.isWSSupported = false;
    } else {
        dderlState.isWSSupported = true;
        var WSUrl = "wss://" + window.location.host + window.location.pathname + "ws";
        console.info("Opening websocket");
        var websocket = new WebSocket(WSUrl);
        websocket.onopen = function(evt) { onOpen(evt); }; 
        websocket.onclose = function(evt) { onClose(evt); }; 
        websocket.onmessage = function(evt) { onMessage(evt); }; 
        websocket.onerror = function(evt) { onError(evt); }; 
        dderlState.websocket = websocket;
    }
}

export function websocket_send(Msg, SuccessCallback, ErrorCallback) {
    console.info("WS sending messgege : ", Msg);
    dderlState.websocket.send(Msg);
    receivers[Msg] = {success : SuccessCallback, error : ErrorCallback};
}

export function disconnect_websocket() {
    if(dderlState.websocket) {
        dderlState.websocket.close();
    }
}

function onOpen(evt) { 
    console.log("WS opened" , evt);
}  
function onClose(evt) { 
    console.log("WS closed", evt);
}  
function onMessage(evt) { 
    console.log("WS got message : " + evt.data);
    process_message(evt.data);
}  
function onError(evt) {
    console.log("WS got error : " + evt.data);
}

function process_message(MsgJSON) {
    var Msg = JSON.parse(MsgJSON);
    var callbacks = receivers[Object.keys(Msg)[0]];
    if (callbacks.success) {
        callbacks.success(Object.values(Msg)[0]);
    }
    console.log("REceivers : ", receivers);
    console.log("Msg : ", Msg);
}