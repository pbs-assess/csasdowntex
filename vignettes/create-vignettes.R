# Source this script to build and install all csasdown vignettes on your machine

root_dir <- here::here()
doc_dir <- file.path(root_dir, "doc")
inst_doc_dir <- file.path(root_dir, "inst/doc")

build_vignettes(root_dir)

# Copy the files created in the `build_vignettes()` call (in `inst/doc` to
# the `doc` directory so that `browseVignettes()` works
dir.create(inst_doc_dir, showWarnings = FALSE)
files <- dir(doc_dir, full.names = TRUE)
# Don't copy .R, .r, .Rmd, or .rmd files
files <- files[-grep("^.*[R|r][md]*$", files)]

fs <- file.copy(files,
                inst_doc_dir,
                overwrite = TRUE)

# Press Enter if prompted to update packages during this install
devtools::install(root_dir)

# Go to the csasdown/doc directory and the vignette PDFs are there or
# browse vignettes with the following command (it doesn't run automatically
# like it should when you source this script):
browseVignettes("csasdown")
