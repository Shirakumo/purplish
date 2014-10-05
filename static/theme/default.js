$(function(){
    $(".id").click(function(){
        var post = $(this).closest(".post");
        $("#replybox").detach().insertAfter(post);
        $("#replybox .text").focus();
        $('html, body').stop().animate({
            scrollTop: post.offset().top
        }, 500);
        return false;
    });
});
