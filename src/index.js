import './main.css';
import './bulma.min.css';
import logoPath from './logo.svg';
const { App } = require('./App.elm');

let scroll = window.pageYOffset || document.body.scrollTop;

const myApp = App.embed(document.getElementById('root'), logoPath);

window.onscroll = function()
{
    var newScroll = window.pageYOffset || document.body.scrollTop;
    console.log("SCROLLING",newScroll);
    myApp.ports.scroll.send([scroll, newScroll]);
    scroll = newScroll;
};

window.setTimeout(
    function() { myApp.ports.researchHeight.send(getScroll(document.getElementById('research'))); },
    100
);

window.onresize = function()
{
    myApp.ports.researchHeight.send(getScroll(document.getElementById('research')));
};

function getScroll(elem) {
    var box = elem.getBoundingClientRect();

    var body = document.body;
    var docEl = document.documentElement;

    var scrollTop = window.pageYOffset || docEl.scrollTop || body.scrollTop;
    // var scrollLeft = window.pageXOffset || docEl.scrollLeft || body.scrollLeft;

    var clientTop = docEl.clientTop || body.clientTop || 0;
    // var clientLeft = docEl.clientLeft || body.clientLeft || 0;

    var top  = box.top +  scrollTop - clientTop;
    // var left = box.left + scrollLeft - clientLeft;

    return Math.round(top);
    // return { top: Math.round(top), left: Math.round(left) };
}
