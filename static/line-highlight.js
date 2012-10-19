$(document).ready(function() {
   $("a.line-link").click(function() {
     $(".lines tr").removeClass("highlight")
       .filter($(this).attr("href"))
       .addClass("highlight");
   });

  if ( true
       && typeof ($("a.line-link:first[href]").get(0)) !== "undefined" ) {
      $("a.line-link:first").trigger('click');
      window.location.href = $("a.line-link:first").attr('href');
  }
});
