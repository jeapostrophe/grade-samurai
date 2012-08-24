$(document).ready(function() {
   $("a.line-link").click(function() {
     $(".line").each(function() {
       $(this).removeClass("highlight");
       });
     $($(this).attr("href")).addClass("highlight");
   });
});
