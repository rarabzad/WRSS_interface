library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body { margin:0; padding:0; height:100%; overflow:hidden; background:#0d1117; }
      #builder-frame { position:fixed; inset:0; width:100%; height:100%; border:none; }
    ")),
    
    # ── Relay: iframe → R (via setInputValue) ──────────────────────────
    # ── Relay: R → iframe (via sendCustomMessage → postMessage) ────────
    tags$script(HTML("
      window.addEventListener('message', function(e) {
        if (!e.data || e.data.source !== 'builder') return;
        Shiny.setInputValue('iframe_msg',
          { raw: JSON.stringify(e.data.payload), ts: Date.now() },
          { priority: 'event' });
      });

      Shiny.addCustomMessageHandler('to_builder', function(msg) {
        var frame = document.getElementById('builder-frame');
        if (frame && frame.contentWindow)
          frame.contentWindow.postMessage(msg, '*');
      });
    "))
  ),
  tags$iframe(
    id  = "builder-frame",
    src = "builder.html"
  )
)