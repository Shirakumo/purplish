$(function (){    
    function zeroPadDate(date){
        return ("0" + date).slice(-2);
    }
    
    function formatDate(date){
        return date.getFullYear()+"."+
            zeroPadDate(date.getMonth())+"."+
            zeroPadDate(date.getDay())+" "+
            zeroPadDate(date.getHours())+":"+
            zeroPadDate(date.getMinutes())+":"+
            zeroPadDate(date.getSeconds());
    }
    
    $("#replybox .time").text(formatDate(new Date()));

    function registerFileRemove(element){
        element.dblclick(function(){
            $(this).remove();
        });
    }

    function registerFileChange(element){
        element.change(function(){
            var li = document.createElement("li");
            var span = document.createElement("span");
            var input = document.createElement("input");
            $(input).attr({"type": "file", "name": "files[]"});
            $(span).text($(this).val().split(/(\\|\/)/g).pop());
            $(li).append(span);
            $(li).append($(this).detach());
            $("#replybox .files .filelist").append(li);
            $("#replybox .files").prepend(input);
            registerFileRemove($(li));
            registerFileChange($(input));
        });
    }
    
    registerFileChange($("#replybox .files>input"));

    $("#themes li").click(function(){
        var theme = $(this).text();
        setCookie("purplish-theme", theme);
        loadTheme(theme);
    });
});
