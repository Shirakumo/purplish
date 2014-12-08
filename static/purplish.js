function Purplish(){
    this.postFetchIntervalID = null;
    this.postFetchInterval = 15000;
    this.currentPost = null;
    this.currentFile = null;
    this.keychords = {};
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

    Purplish.prototype.nextOfKind = function(kind, thing){
        var things = $(kind);
        for(var i=0; i<things.length; i++){
            if(things[i] == thing){
                return things[i+1] || null;
            }
        }
        return null;
    }

    Purplish.prototype.previousOfKind = function(kind, thing){
        var things = $(kind);
        for(var i=things.length-1; i>=0; i--){
            if(things[i] == thing){
                return things[i-1] || null;
            }
        }
        return null;
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

    Purplish.prototype.rotateHeader = function(){
        return $("#header-image").attr("src", "/api/purplish/header?t="+new Date().getTime());
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

    Purplish.prototype.currentBoard = function(){
        return $("#replybox input[name='board']").val();
    }

    Purplish.prototype.getPost = function(id){
        var post = $(".post[data-post-id="+id+"]");
        if (post.length == 0)
            purplish.log("[Warn] No post with ID",id,"found");
        return post;
    }

    Purplish.prototype.getThread = function(id){
        var post = $(".thread[data-thread-id="+id+"]");
        if (post.length == 0)
            purplish.log("[Warn] No thread with ID",id,"found");
        return post;
    }
    
    Purplish.prototype.getFile = function(id){
        var file = $(".file[data-file-id="+id+"]");
        if(file.length == 0)
            purplish.log("[Warn] No file with ID",id,"found");
        return file;
    }
    
    Purplish.prototype.ensurePost = function(thing){
        if(thing instanceof jQuery && thing.hasClass("post")){
            return thing;
        }else if(thing instanceof Element && $(thing).hasClass("post")){
            return $(thing);
        }else if(parseInt(Number(thing)) == thing){
            return purplish.getPost(thing);
        }else{
            purplish.log("[Warn] Don't know how to ensure",thing,"to a post");
            return thing;
        }
    }
    
    Purplish.prototype.ensureThread = function(thing){
        if(thing instanceof jQuery && thing.hasClass("thread")){
            return thing;
        }else if(thing instanceof Element && $(thing).hasClass("thread")){
            return $(thing);
        }else if(parseInt(Number(thing)) == thing){
            return purplish.getThread(thing);
        }else{
            purplish.log("[Warn] Don't know how to ensure",thing,"to a thread");
            return thing;
        }
    }
    
    Purplish.prototype.ensureFile = function(thing){
        if(thing instanceof jQuery && thing.hasClass("file")){
            return thing;
        }else if(thing instanceof Element && $(thing).hasClass("file")){
            return $(thing);
        }else if(parseInt(Number(thing)) == thing){
            return purplish.getFile(thing);
        }else{
            purplish.log("[Warn] Don't know how to ensure",thing,"to a file");
            return thing;
        }
    }
    
    Purplish.prototype.previousPost = function(thing){
        return purplish.previousOfKind(".post", purplish.ensurePost(thing || purplish.currentPost)[0]);
    }
    
    Purplish.prototype.nextPost = function(thing){
        return purplish.nextOfKind(".post", purplish.ensurePost(thing || purplish.currentPost)[0]);
    }
    
    Purplish.prototype.postFiles = function(thing){
        return $(".files .file",purplish.ensurePost(thing || purplish.currentPost));
    }

    Purplish.prototype.postThread = function(thing){
        return purplish.ensurePost(thing || purplish.currentPost).closest(".thread");
    }
    
    Purplish.prototype.filePost = function(thing){
        return purplish.ensureFile(thing || purplish.currentFile).closest(".post");
    }
    
    Purplish.prototype.previousFile = function(thing){
        return purplish.previousOfKind(".file", purplish.ensureFile(thing || purplish.currentFile)[0]);
    }

    Purplish.prototype.nextFile = function(thing){
        return purplish.nextOfKind(".file", purplish.ensureFile(thing || purplish.currentFile)[0]);
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
        var post = purplish.ensurePost(post);
        if($(".posts .post[data-post-id="+post.data("post-id")+"]").length == 0){
            var li = $(document.createElement("li"));
            $(".posts").append(li.append(post));
            purplish.registerPost(post);
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
                        purplish.integratePost($($.parseHTML(post.trim())));
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

    Purplish.prototype.setCurrentFile = function(thing, changePost){
        if(typeof(changePost)==='undefined')changePost=true;
        var file = purplish.ensureFile(thing);
        purplish.log("Setting current file",file.data("file-id"));
        if(file && changePost){
            purplish.setCurrentPost(purplish.filePost(file), false);
        }
        purplish.currentFile = file;
        $(".file.highlight").removeClass("highlight");
        $(file).addClass("highlight");
        $(document).trigger("purplish-current-file-changed", file);
        return file;
    }

    Purplish.prototype.setCurrentPost = function(thing, changeFile){
        if(typeof(changeFile)==='undefined')changeFile=true;
        var post = purplish.ensurePost(thing);
        purplish.log("Setting current post",post.data("post-id"));
        purplish.currentPost = post;
        if(changeFile){
            purplish.setCurrentFile($(purplish.postFiles(post)[0]), false);
        }
        $(".post.highlight").removeClass("highlight");
        $(".post[data-post-id="+post.data("post-id")+"]").addClass("highlight");
        $(document).trigger("purplish-current-post-changed", post);
        return post;
    }

    Purplish.prototype.highlightPost = function(thing){
        var post = purplish.ensurePost(thing);
        purplish.log("Highlighting post",post.data("post-id"));
        return purplish.setCurrentPost(post);
    }

    Purplish.prototype.highlightFile = function(thing){
        var file = purplish.ensureFile(thing);
        purplish.log("Highlighting file",file.data("file-id"));
        return purplish.setCurrentFile(file);
    }

    Purplish.prototype.jumptoBoard = function(id){
        purplish.log("Going to board",id);
        window.location = "/board/"+id;
    }

    Purplish.prototype.gotoPost = function(thing){
        var post = purplish.ensurePost(thing);
        if(post){
            if(post.length==0){
                purplish.jumptoPost(thing);
            }else{
                purplish.log("Scrolling to post",post.data("post-id"));
                purplish.highlightPost(post);
                $('html, body').stop().animate({
                    scrollTop: $(">a[name]",post).offset().top
                }, 200);
            }
        }
        return post;
    }

    Purplish.prototype.jumptoPost = function(id){
        purplish.log("Jumping to post",id);
        window.location = "/post/"+id;
    }

    Purplish.prototype.gotoThread = function(id){
        var thread = purplish.ensureThread(id);
        if(thread){
            if(thread.length==0){
                purplish.jumptoThread(id);
            }else{
                purplish.log("Scrolling to thread",thread.data("thread-id"));
                purplish.setCurrentPost($(".post",thread)[0]);
                $('html, body').stop().animate({
                    scrollTop: $("a[name]",thread).offset().top
                }, 200);
            }
        }
        return thread;
    }

    Purplish.prototype.jumptoThread = function(id){
        var thread = purplish.ensureThread(id);
        id = (thread)? thread.data("thread-id") : id;
        purplish.log("Jumping to thread",id);
        window.location = "/thread/"+id;
    }

    Purplish.prototype.gotoFile = function(thing){
        var file = purplish.ensureFile(thing);
        if(file){
            purplish.log("Scrolling to file",file);
            purplish.highlightFile(file);
            $('html, body').stop().animate({
                scrollTop: file.offset().top-50
            }, 200);
        }
        return file;
    }

    Purplish.prototype.gotoPostBox = function(){
        $('html, body').stop().animate({
            scrollTop: $("#replybox").offset().top-50
        }, 200);
        $("#replybox textarea").focus();
        return $("#replybox");
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
        });
    }

    Purplish.prototype.bindKey = function(key, func, explanation){
        Mousetrap.bind(key, function(){
            purplish.log("Triggering key func",key);
            func()});
        purplish.keychords[key] = explanation || "?";
    }

    Purplish.prototype.popup = function(title, content, type){
        if($(".popup").length == 0){
            if(typeof(type)==='undefined')type="info";
            var el = document.createElement("div");
            var inner = document.createElement("div");
            var h1 = document.createElement("h1");
            $(h1).text(title);
            $(inner).addClass("inner")
                .html(content)
                .prepend(h1)
                .click(function(){return false;});
            $(el).addClass(type)
                .addClass("popup")
                .append(inner)
                .click(function(){$(this).remove();})
                .appendTo("body");
            return el;
        }else{
            $(".popup").click();
            return null;
        }
    }

    Purplish.prototype.showKeychords = function(){
        var el = document.createElement("div");
        var inner = "";
        for(var key in purplish.keychords){
            inner = inner+"<div class='chord'><code>"+key+"</code><span>"+purplish.keychords[key]+"</span></div>";
        }
        return purplish.popup("Keychords", inner);
    }

    Purplish.prototype.initKeychords = function(){
        purplish.bindKey("h",purplish.rotateHeader,
                        "Rotate the header.");
        purplish.bindKey("p",purplish.gotoPostBox,
                        "Jump to the post box.");
        purplish.bindKey("n",function(){$("html,body").scrollTop(0);},
                        "Jump to the top of the page.");
        purplish.bindKey("m",function(){$("html,body").scrollTop($(document).height());},
                        "Jump to the bottom of the page.");
        purplish.bindKey("w",function(){purplish.gotoPost(purplish.previousPost());},
                        "Go to the previous post.");
        purplish.bindKey("s",function(){purplish.gotoPost(purplish.nextPost());},
                        "Go to the next post.");
        purplish.bindKey("a",function(){purplish.gotoFile(purplish.previousFile());},
                        "Go to the previous file.");
        purplish.bindKey("d",function(){purplish.gotoFile(purplish.nextFile());},
                        "Go to the next file.");
        purplish.bindKey("q",function(){history.back()},
                        "Go backwards in the browser history.");
        purplish.bindKey("e",function(){history.forward()},
                        "Go forwards in the browser history.");
        purplish.bindKey("b",function(){purplish.jumptoBoard(purplish.currentBoard());},
                        "Go to the board.");
        purplish.bindKey("v",function(){purplish.jumptoThread(purplish.postThread());},
                        "Go to the thread of the current post.");
        purplish.bindKey("?",purplish.showKeychords,
                        "Show keychords cheat sheet.");

        // We have to override this because the default implementation
        // is broken.
        Mousetrap.stopCallback = function(e, element, combo) {
            return element.tagName == 'input' || element.tagName == 'select' || element.tagName == 'textarea' ||
                (element.contentEditable && element.contentEditable == 'true');
        }
        purplish.log("Keychords ready");
    }

    Purplish.prototype.init = function(){
        purplish.log("Init...");

        // Remote comps init
        $(document).trigger("purplish-init-start");

        // Header rotate
        $("#header-image").click(purplish.rotateHeader);

        // Register existing posts
        $(".post").each(function(){purplish.registerPost($(this));});

        purplish.setCurrentPost($(".post")[0]);
        
        // Highlight linked
        var hash = window.location.hash;
        if(hash.indexOf("#post-")==0){
            purplish.highlightPost(hash.slice("#post-".length));
        }

        // Keychords
        purplish.initKeychords();

        // Posbox
        purplish.initPostBox($("#replybox"));

        // Theme picking
        $("#themes li").click(function(){
            var theme = $(this).text();
            purplish.setTheme(theme);
            loadTheme(theme);
        });

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
