
rmarkdown::pandoc_convert(
  input   = here::here("inst/csas-docx/FSAR-PAS2025-eng.docx"),
  to      = "markdown",
  output  = here::here("inst/csas-docx/FSAR-PAS2025-eng.md"),
  options = c("--wrap=none")
)

tmp <- readLines("inst/csas-docx/FSAR-PAS2025-eng.md")
tmp <- stringi::stri_replace_all_fixed(tmp, c("\\[", "\\]"), c("[", "]"), vectorise_all = FALSE)
tmp <- stringi::stri_replace_all_fixed(tmp, "\u00A0", "&nbsp;", vectorise_all = FALSE)
writeLines(tmp, "inst/csas-docx/FSAR-PAS2025-eng.md")


rmarkdown::pandoc_convert(
  input   = here::here("inst/csas-docx/FSAR-PAS2025-fra.docx"),
  to      = "markdown",
  output  = here::here("inst/csas-docx/FSAR-PAS2025-fra.md"),
  options = c("--wrap=none")
)

tmp <- readLines("inst/csas-docx/FSAR-PAS2025-fra.md")
tmp <- stringi::stri_replace_all_fixed(tmp, c("\\[", "\\]"), c("[", "]"), vectorise_all = FALSE)
tmp <- stringi::stri_replace_all_fixed(tmp, "\u00A0", "&nbsp;", vectorise_all = FALSE)
writeLines(tmp, "inst/csas-docx/FSAR-PAS2025-fra.md")
