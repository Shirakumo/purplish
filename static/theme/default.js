$(document).on("purplish-init-start", function(){    
    $("#header-image").click(function(){
        $(this).attr("src", "/api/purplish/header?t="+new Date().getTime());
    });

    function refitPreview(){
        $(".preview.full").each(function(){
            var height = $(this).innerHeight();
            $("img,video",this).each(function(){
                $(this).attr("style", "max-height:"+(height-40)+"px !important;");
            });
        });
        return null;
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
                // Show inline
                $(this).removeClass("full");
                // Make clicking propagate to the preview again
                $("*:not(a)",this).unbind("click");
            }else{
                // Show fullscreen
                $(this).addClass("full");
                // Autoplay video, audio
                $("audio,video",this).each(function(){this.play();});
                // Prevent click closing it
                $("*:not(a)",this).click(function(e){
                    e.stopPropagation();
                });
                // Full image
                $(".image",this).each(function(){
                    $("img",this).attr("src", $(this).attr("href"));
                });
                // Refit
                refitPreview();
            }
        });
    });
    
    $(window).resize(function(){
        refitPreview();
    });
});
