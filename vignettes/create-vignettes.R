# Source this script to build and install all csasdown vignettes on your machine

root_dir <- here::here()
inst_doc_dir <- here::here("inst/doc")

build_vignettes(root_dir)
dir.create(inst_doc_dir, showWarnings = FALSE)
file.copy(dir("doc", full.names = TRUE),
          inst_doc_dir,
          overwrite = TRUE)
devtools::install(root_dir)

utils::browseVignettes("csasdown")
