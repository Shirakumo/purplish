function getCookie(name) {
    var value = "; " + document.cookie;
    var parts = value.split("; " + name + "=");
    if (parts.length == 2) return parts.pop().split(";").shift();
}

function setCookie(name, value){
    var date = new Date();
    date.setTime(date.getTime() + 100*365*24*60*60*1000);
    document.cookie = name+"="+value+";expires="+date.toGMTString()+";path=/";
}

function loadCSS(filename){
    var link=document.createElement("link");
    link.setAttribute("class", "theme-part");
    link.setAttribute("rel", "stylesheet");
    link.setAttribute("type", "text/css");
    link.setAttribute("href", filename);
    document.getElementsByTagName("head")[0].appendChild(link);
}

function loadJS(filename){
    var script=document.createElement("script");
    script.setAttribute("class", "theme-part");
    script.setAttribute("type", "text/javascript");
    script.setAttribute("src", filename);
    window.onload = function(){
        document.getElementsByTagName("body")[0].appendChild(script);
    }
}

function loadTheme(name){
    var els = document.getElementsByClassName("theme-part");
    for(var i=0; i<els.length; i++){
        els[i].parentNode.removeChild(els[i]);
    }
    loadCSS("/static/purplish/theme/"+name+".css");
    loadJS("/static/purplish/theme/"+name+".js");
}

var theme = getCookie("purplish-theme");
if(theme !== undefined && theme !== ""){
    loadTheme(theme);
}
