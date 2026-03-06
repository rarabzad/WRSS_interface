# WRSS Interface

[Live URL](https://raventools-wrss.share.connect.posit.cloud)

## Folder structure

```
wrss_shiny/
├── sample_data          ← sample WRSS model and GIS files
├── ui.R          ← Shiny UI (serves the HTML builder in an iframe)
├── server.R      ← R server (runs WRSS simulation, returns results as JSON)
└── www/
    └── builder.html  ← Full interactive builder UI
```
