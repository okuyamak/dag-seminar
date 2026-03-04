
dir.create("slides", showWarnings = FALSE)

file.copy("styles.css", "slides/styles.css", overwrite = TRUE)
file.copy("NEXTBRIDGE.png", "slides/NEXTBRIDGE.png", overwrite = TRUE)

rmarkdown::render(
  "dag_workplace_lbp.Rmd",
  output_dir = "slides"
)