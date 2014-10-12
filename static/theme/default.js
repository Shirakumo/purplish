$(function(){
    $(".post .id").click(function(){
        var post = $(this).closest(".post");
        $("#replybox").detach().insertAfter(post);
        $("#replybox .text").focus();
        $('html, body').stop().animate({
            scrollTop: post.offset().top
        }, 500);
    });

    $("#header-image").click(function(){
        $(this).attr("src", "/api/purplish/header?t="+new Date().getTime());
    });

    // File preview
    $(".preview>a").click(function(e){$(this).parent().click();return false;});
    $(".preview").click(function(){
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
                $("img",this).attr("src", $(this).attr("href"))
                    .attr("style", "max-height:"+$(window).height()+"px !important;width:auto !important");
            });
        }
    });
});
