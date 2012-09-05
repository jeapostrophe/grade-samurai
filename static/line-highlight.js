$(document).ready(function() {
   $("a.line-link").click(function() {
     $(".line").each(function() {
       $(this).removeClass("highlight");
       });
     $($(this).attr("href")).addClass("highlight");
   });


  $("a.line-link:first").trigger('click');
  window.location.href = $("a.line-link:first").attr('href');
});
