function Purplish(){
    this.postFetchIntervalID = null;
    this.postFetchInterval = 15000;
}
var purplish = null;

$(function (){
    Purplish.prototype.log = function(message){
        var args = $.extend([], arguments);
        args.unshift("[Purplish]");
        console.log.apply(console, args);
        return null;
    }
    
    Purplish.prototype.zeroPadDate = function(date){
        return ("0" + date).slice(-2);
    }

    Purplish.prototype.formatDate = function(date){
        return date.getFullYear()+"."+
            purplish.zeroPadDate(date.getMonth()+1)+"."+
            purplish.zeroPadDate(date.getDate())+" "+
            purplish.zeroPadDate(date.getHours())+":"+
            purplish.zeroPadDate(date.getMinutes())+":"+
            purplish.zeroPadDate(date.getSeconds());
    }

    Purplish.prototype.saveName = function(name){
        purplish.log("Saving name", name);
        return setCookie("purplish-name", name);
    }

    Purplish.prototype.getName = function(){
        return getCookie("purplish-name");
    }

    Purplish.prototype.saveTheme = function(theme){
        purplish.log("Saving theme", theme);
        return setCookie("purplish-theme", theme);
    }

    Purplish.prototype.getTheme = function(){
        return getCookie("purplish-theme");
    }

    Purplish.prototype.callRadianceApi = function(url, data, success){
        purplish.log("Calling API", url, "with", data);
        var reldata = $.extend({}, data);
        reldata["data-format"] = "json";
        $.ajax({
            "url": url,
            "data": reldata,
            "success": function(result){
                success(result.data);
            },
            "dataType": "json"
        });
        return null;
    }
    
    Purplish.prototype.callWithThreadPostIDs = function(thread, callback){
        return this.callRadianceApi("/api/purplish/thread/post-ids",
                                    {"thread": thread},
                                    callback);
    }

    Purplish.prototype.callWithRenderedPost = function(post, callback){
        purplish.log("Requesting post "+post);
        return this.callRadianceApi("/api/purplish/post/render",
                                    {"post": post},
                                    callback);
    }

    Purplish.prototype.localPostIDs = function(){
        return $(".post").map(function(){return $(this).data("post-id");});
    }

    Purplish.prototype.localThreadID = function(){
        var id = $("main.thread").data("thread-id");
        return (id == undefined)? null : id;
    }

    Purplish.prototype.integratePost = function(post){
        var post = $($.parseHTML(post));
        if($(".posts .post[data-post-id="+post.data("post-id")+"]").length == 0){
            var li = $(document.createElement("li"));
            $(".posts").append(li.append(post));
        }
        return post;
    }

    Purplish.prototype.fetchNewPosts = function(){
        var thread = purplish.localThreadID();
        var localPosts = purplish.localPostIDs();
        purplish.callWithThreadPostIDs(
            thread,
            function(remotePosts){
                var inexistent = $(remotePosts).not(localPosts).get();
                purplish.log("Found", inexistent.length, "new posts");
                // Convoluted self-calling bullshit in order to work around
                // the dumb callback chain issue that would make it possible
                // for multiple posts to show up out of sequence.
                var i = 0;
                var callFunc = null;
                callFunc = function(post){
                    if(post != null){
                        purplish.integratePost(post);
                    }
                    if(i < inexistent.length){
                        purplish.callWithRenderedPost(inexistent[i],callFunc);
                    }
                    i++;
                };
                callFunc(null);
            });
        return null;
    }

    Purplish.prototype.startPostFetching = function(){
        if(purplish.localThreadID()){
            purplish.log("Starting post fetching");
            purplish.postFetchIntervalID =
                purplish.postFetchIntervalID ||
                setInterval(purplish.fetchNewPosts,
                            purplish.postFetchInterval);
        }
        return purplish.postFetchIntervalID;
    }

    Purplish.prototype.stopPostFetching = function(){
        purplish.log("Stopping post fetching");
        clearInterval(purplish.postFetchIntervalID);
        purplish.postFetchIntervalID = null;
        return null;
    }

    Purplish.prototype.init = function(){
        purplish.log("Init...");
        // Set datetime
        $("#replybox .time").text(purplish.formatDate(new Date()));
        
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
            purplish.setTheme(theme);
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
            $("#replybox .author").val(purplish.getName());
        }
        
        $("#replybox input[type=submit]").click(function(){
            purplish.setName($("#replybox .author").val());
        });

        // Post fetching
        purplish.startPostFetching();

        purplish.log("Init complete.");
        return null;
    }

    purplish = new Purplish();
    purplish.init();
});
