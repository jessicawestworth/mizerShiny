// inst/app/www/app.js
(function(){
  function initPopovers(){
    document.querySelectorAll('[data-bs-toggle="popover"]').forEach(function(el){
      // Remove any old instance
      var old = bootstrap.Popover.getInstance(el);
      if(old) old.dispose();
      // Create new
      new bootstrap.Popover(el, {
        container: 'body',
        html: true,
        sanitize: false,
        trigger: 'hover focus'
      });
    });
  }

  // Fire once when DOM is ready
  document.addEventListener('DOMContentLoaded', initPopovers);

  // And again after Shiny renders/upgrades the UI
  document.addEventListener('shiny:connected', initPopovers);
  document.addEventListener('shiny:value',     initPopovers);
  document.addEventListener('shown.bs.tab',     initPopovers);
})();
