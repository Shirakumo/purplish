$(function (){
    // Postbox date
    function zeroPadDate(date){
        return ("0" + date).slice(-2);
    }
    
    function formatDate(date){
        return date.getFullYear()+"."+
            zeroPadDate(date.getMonth()+1)+"."+
            zeroPadDate(date.getDate())+" "+
            zeroPadDate(date.getHours())+":"+
            zeroPadDate(date.getMinutes())+":"+
            zeroPadDate(date.getSeconds());
    }
    
    $("#replybox .time").text(formatDate(new Date()));

    // File handling
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

    // Theme picking
    $("#themes li").click(function(){
        var theme = $(this).text();
        setCookie("purplish-theme", theme);
        loadTheme(theme);
    });

    // Opening inline images
    $(".text img").css("cursor", "pointer").click(function(){
        window.open($(this).attr("src"),'_blank');
    });

    // Posting things
    $(".post .id").click(function(){
        var post = $(this).closest(".post");
        var thread = $(this).closest(".thread");
        var box = $("#replybox");
        if(box.length>0){
            // Update post target
            box.attr("action","/api/purplish/post/create");
            $("input[type=submit]",box).val("Post");
            $("input[name=thread]",box).val(thread.data("thread-id"));
            $(".title,.text,.files input",box).removeAttr("required");
            // Update textbox
            var text = $(".text", box);
            text.focus();
            text.text((text.text()+"\n\n"+">>"+post.data("post-id")).trim()+"  \n");
            return false;
        }
    });

    // Name saving
    if(getCookie("purplish-username") !== undefined){
        $("#replybox .author").val(getCookie("purplish-username"));
    }
    
    $("#replybox input[type=submit]").click(function(){
        setCookie("purplish-username", $("#replybox .author").val());
    });
});
