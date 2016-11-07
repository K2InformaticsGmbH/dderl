// !+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!+-+-+!
//src - https://codepen.io/yashbhardwaj/pen/hHgtE
import $ from 'jquery';
var screenDiv;
export function startScreensaver(){
    screenDiv = $('<div class="screensaver"></div>');
    $('#main-body').prepend(screenDiv);
}

export function stopScreensaver() {
    screenDiv.remove();
}
