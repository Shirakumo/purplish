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
});
