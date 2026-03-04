library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { margin:0; padding:0; overflow:hidden; height:100vh; background:#0d1117; }
      #builder-frame { width:100vw; height:100vh; border:none; display:block; }
    "))
  ),

  # The entire builder UI is an iframe pointing at www/builder.html
  # Messages flow via postMessage ↔ Shiny custom handlers
  tags$iframe(
    id        = "builder-frame",
    src       = "builder.html",
    width     = "100%",
    height    = "100%",
    frameborder = "0"
  ),

  # Bridge: forward postMessages from the iframe into Shiny inputs
  tags$script(HTML("
    window.addEventListener('message', function(e) {
      if (!e.data || !e.data.type) return;
      Shiny.setInputValue('iframe_msg', e.data, {priority: 'event'});
    });

    // Forward messages FROM Shiny TO the iframe
    Shiny.addCustomMessageHandler('to_builder', function(msg) {
      var iframe = document.getElementById('builder-frame');
      if (iframe && iframe.contentWindow) {
        iframe.contentWindow.postMessage(msg, '*');
      }
    });
  "))
)
