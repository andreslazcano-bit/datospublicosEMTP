// JS para habilitar/deshabilitar el botón de descarga de minutas
Shiny.addCustomMessageHandler('toggleDownloadBtn', function(message) {
  var btnIds = ['descargar_minuta', 'descargar_minuta_pdf', 'descargar_minuta_excel'];
  btnIds.forEach(function(id){
    var btn = document.getElementById(id);
    if (btn) {
      btn.disabled = !message.enabled;
      if (btn.disabled) {
        btn.classList.add('btn-disabled');
        btn.style.opacity = '0.6';
        btn.style.cursor = 'not-allowed';
      } else {
        btn.classList.remove('btn-disabled');
        btn.style.opacity = '1';
        btn.style.cursor = 'pointer';
      }
    }
  });
});
