$(document).on("purplish-init-start", function(){
    function refitPreview(){
        $(".preview.full").each(function(){
            var height = $(this).innerHeight();
            $("img,video",this).each(function(){
                $(this).attr("style", "max-height:"+(height-40)+"px !important;");
            });
        });
        return null;
    }

    function exitPreview(thing){
        var preview = ($(thing).hasClass("preview"))? thing : $(".preview", (thing || document));
        // Show inline
        $(preview).removeClass("full");
        // Make clicking propagate to the preview again
        $("*:not(a)",preview).unbind("click");
    }

    function enterPreview(thing){
        var preview = ($(thing).hasClass("preview"))? thing : $(".preview", (thing || purplish.currentFile));
        exitPreview();
        // Show fullscreen
        $(preview).addClass("full");
        // Autoplay video, audio
        $("audio,video",preview).each(function(){preview.play();});
        // Prevent click closing it
        $("*:not(a)",preview).click(function(e){
            e.stopPropagation();
        });
        // Full image
        $(".image",preview).each(function(){
            $("img",preview).attr("src", $("a",preview).attr("href"));
        });
        // Refit
        refitPreview();
    }

    $(document).on("register-post", function(e, post){
        // Inserting
        $(".post .id", post).click(function(){
            var post = $(this).closest(".post");
            $("#replybox").detach().insertAfter(post);
            $("#replybox .text").focus();
            purplish.gotoPost(post);
        });

        // File preview
        $(".preview>a", post).click(function(e){$(this).parent().click();return false;});
        $(".preview", post).click(function(){
            if($(this).hasClass("full")){
                exitPreview(this);
            }else{
                enterPreview(this);
            }
        });
    });

    $(document).on("purplish-current-file-changed", function(e, file){
        if($(".preview.full").length != 0){
            purplish.log("Simulating preview switch...");
            $(".preview",file).click();
        }
    });

    purplish.bindKey("escape", exitPreview,
                     "Exit the preview");
    purplish.bindKey("return", enterPreview,
                     "Enter preview mode for the current file.");
    
    $(window).resize(function(){
        refitPreview();
    });
});
