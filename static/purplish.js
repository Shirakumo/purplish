function Purplish(){
    this.postFetchIntervalID = null;
    this.postFetchInterval = 15000;
}
var purplish = null;

$(document).trigger("purplish-genesis");

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

    Purplish.prototype.getPost = function(id){
        var post = $(".post[data-post-id="+id+"]");
        if (post.length == 0)
            purplish.log("[Warn] No post with ID",id,"found");
        return post;
    }

    Purplish.prototype.ensurePost = function(thing){
        if (thing instanceof jQuery){
            if(thing.length == 0)
                purplish.log("[Warn] Ensuring 0-length jQuery object",thing,"to post");
            return thing;
        }else if (parseInt(Number(thing)) == thing){
            return purplish.getPost(thing);
        }else{
            purplish.log("[Warn] Don't know how to ensure",thing,"to a post");
            return thing;
        }
    }

    Purplish.prototype.localPostIDs = function(){
        return $(".post").map(function(){return $(this).data("post-id");});
    }

    Purplish.prototype.localThreadID = function(){
        var id = $("main.thread").data("thread-id");
        return (id == undefined)? null : id;
    }
    
    Purplish.prototype.registerPost = function(post){
        var post = purplish.ensurePost(post);
        purplish.log("Registering post", post.data("post-id"));

        // Ensure we're starting fresh.
        post.unbind();
        $("*",post).unbind();
        
        // ID click
        $(".id", post).click(function(){
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
                text.val((text.val()+"\n\n"+">>"+post.data("post-id")).trim()+"  \n");
                return false;
            }
        });

        // Set datetime
        $("time", post).each(function(){
            $(this).text(purplish.formatDate(new Date(Date.parse($(this).attr("datetime")))));
        });
        
        // Post refs
        $(".post-reference", post).click(function(){
            purplish.gotoPost($(this).text().slice(2));
            return false;
        });
        
        // Opening inline images
        $(".text img", post).css("cursor", "pointer").click(function(){
            window.open($(this).attr("src"),'_blank');
        });

        $(document).trigger("register-post", [post]);
        
        return post;
    }

    Purplish.prototype.integratePost = function(post){
        var post = $($.parseHTML(post));
        if($(".posts .post[data-post-id="+post.data("post-id")+"]").length == 0){
            purplish.registerPost(post);
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

    Purplish.prototype.highlightPost = function(id){
        var post = purplish.ensurePost(id);
        purplish.log("Highlighting post",post.data("post-id"));
        $(".post.highlight").removeClass("highlight");
        $(".post[data-post-id="+post.data("post-id")+"]").addClass("highlight");
    }

    Purplish.prototype.gotoPost = function(id){
        var post = purplish.ensurePost(id);
        if(post.length==0){
            purplish.log("Going to post",id);
            window.location = "/post/"+id;
        }else{
            purplish.log("Scrolling to post",post.data("post-id"));
            purplish.highlightPost(id);
            $('html, body').stop().animate({
                scrollTop: $(">a[name]",post).offset().top
            }, 200);
        }
    }

    Purplish.prototype.initPostBox = function(box){
        // Set datetime
        $(".time", box).text(purplish.formatDate(new Date()));

        // File handling
        function registerFileRemove(element){
            element.dblclick(function(){
                $(this).remove();
            }).click(function(){
                $(this).toggleClass("selected");
            });
        }
        
        function registerFileChange(element){
            element.change(function(){
                // Create subsitute input
                var input = document.createElement("input");
                $(input).attr({"type":"file", "name":"files[]", "multiple":"multiple"});
                $(".files", box).prepend(input);

                // Create label for list
                var span = document.createElement("span");
                var names = [];
                for(var i=0; i<this.files.length; i++)
                    names.push(this.files[i].name);
                $(span).html(names.join("<br />"));

                // Create list item
                var li = document.createElement("li");
                $(li).append(span);
                $(li).append($(this).detach());
                $(".files .filelist", box).append(li);
                
                // Reregister for new element
                registerFileRemove($(li));
                registerFileChange($(input));
            });
        }
        
        registerFileChange($(".files>input", box));
        
        // Name stuff
        var name = purplish.getName();
        if(name !== undefined){
            $(".author", box).val(name);
        }
        
        $("input[type=submit]", box).click(function(){
            purplish.saveName($(".author", box).val());
            return false;
        });
    }

    Purplish.prototype.init = function(){
        purplish.log("Init...");

        // Remote comps init
        $(document).trigger("purplish-init-start");

        // Register existing posts
        $(".post").each(function(){purplish.registerPost($(this));});
        
        // Highlight linked
        var hash = window.location.hash;
        if(hash.indexOf("#post-")==0){
            purplish.highlightPost(hash.slice("#post-".length));
        }

        // Theme picking
        $("#themes li").click(function(){
            var theme = $(this).text();
            purplish.setTheme(theme);
            loadTheme(theme);
        });

        // Posbox
        purplish.initPostBox($("#replybox"));

        // Post fetching
        purplish.startPostFetching();

        // Remote comps init
        $(document).trigger("purplish-init-finish");

        purplish.log("Init complete.");
        return null;
    }

    purplish = new Purplish();

    // We have to wait for the theme script to load.
    $(document).on("script-loaded",function(){
        purplish.init();
    });
});
