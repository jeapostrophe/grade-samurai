$(document).ready(function() {
   $("a.line-link").click(function() {
     $(".line").each(function() {
       $(this).removeClass("highlight");
       });
     $($(this).attr("href")).addClass("highlight");
   });

  if ( true
       && typeof ($("a.line-link:first")) !== "undefined"
       && typeof ($("a.line-link:first").attr('href')) !== "undefined" ) {
      $("a.line-link:first").trigger('click');
      window.location.href = $("a.line-link:first").attr('href');
  }
});
