#' csasdown: A package for creating CSAS Research Document with R Markdown/bookdown
#'
#' Uses the 'bookdown' R package to generate CSAS (Canadian Science Advisory
#' Secretariat) Research Documents, Science Responses, and Technical Reports in
#' PDF or Word format. The package is based on Chester Ismay's thesisdown
#' package and Ben Marwick's huskydown package.
#'
#' @name csasdown
#' @keywords internal
"_PACKAGE"
NULL

# Shared message about PDF document acceptance
csas_pdf_message <- paste(
  "*CSAS is no longer accepting PDF versions of Research Documents or Science Responses.*",
  "We are hoping that documents already finished as PDFs will be accepted, but we *do not*",
  "recommend starting a new report as a PDF. You can try Research Document .docx output with",
  "csasdown2 (https://github.com/pbs-assess/csasdown2). We have worked with the CSAS office to ensure",
  "that version is fully compliant with current CSAS formatting standards.",
  "Tech Reports can still be submitted as PDFs because they",
  "are not associated with CSAS."
)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(csas_pdf_message)
}

if(getRversion() >= "2.15.1")
  utils::globalVariables(c("region_info", ".x", ".y",
                           "fn", "num_lines", "post_num",
                           "pre_num"))