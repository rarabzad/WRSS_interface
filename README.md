# WRSS Interface

[Live url](https://raventools-wrss.share.connect.posit.cloud)

## Folder structure

```
wrss_shiny/
├── sample_data          ← sample WRSS model and GIS files
├── ui.R          ← Shiny UI (serves the HTML builder in an iframe)
├── server.R      ← R server (runs WRSS simulation, returns results as JSON)
└── www/
    └── builder.html  ← Full interactive builder UI
```

## How to deploy on Posit Cloud

1. Log in to **posit.cloud** and create a new **Shiny App** project.
2. Upload the three files above, preserving the `www/` subfolder.
3. Open the R console in Posit Cloud and run:
   ```r
   install.packages("WRSS")
   install.packages("jsonlite")   # usually pre-installed
   ```
4. Click **Run App** — the builder opens full-screen.

## How it works

```
Browser (builder.html)
      │  postMessage({ type: 'run_model', model: {...} })
      ▼
Shiny ui.R   →  Shiny server.R
                  • builds WRSS objects from model JSON
                  • calls sim(Area)
                  • serialises result with jsonlite::toJSON()
      │  sendCustomMessage('to_builder', { type: 'run_result', result: {...} })
      ▼
Browser (builder.html)
  • parses result
  • renders KPI cards + Chart.js time-series plots in Results tab
```

## User workflow (all in one tab)

1. Set Area name, dates, interval in the top bar.
2. Select an object type from the left toolbar → click map to place.
3. Fill in properties in the right panel.
4. Use **Connect** to draw typed links between nodes.
5. Click **▶ Run Model** — a spinner appears while R simulates.
6. Results appear automatically in the **Results** tab with charts and KPIs.
7. Download charts as JPEG or node data as CSV/TXT.

## Troubleshooting

| Symptom | Fix |
|---|---|
| Status bar shows "R: offline" | App isn't running as a Shiny app — open via `runApp()` or Posit Cloud |
| Status bar shows "WRSS not installed" | Run `install.packages("WRSS")` in the Posit console |
| Simulation error shown | Check the error message; usually a missing required field |
| Results tab empty after run | Check browser console for JSON parse errors |
