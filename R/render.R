#' Render any csasdown document with bilingual features
#'
#' @description
#' Render a csasdown document with bilingual features. Renders a csasdown
#' document (autodetects resdoc, sr, techreport, manureport) using the
#' [bookdown::render_book()] method but includes a pre-processing step to
#' do several things (simplified):
#' 1. Create temporary files for `_bookdown.yml`, and the files listed
#'    inside it
#' 2. Inject the temporary version of `index.Rmd` with special code to
#'    allow bilingual features to be used properly
#' 3. Convert anything inside [cat()] calls to cat-like strings instead
#'    of rmarkdown strings.
#' This means that any inline R code included with backticks,
#' eg: `` `r Sys.time()` `` will be replaced with
#' a quoted, comma separated string (see [catize()]). This allows the [cat()]
#' function inside a code chunk to contain backtick-quoted R expressions
#' exactly like what knitr processes inline.
#'
#' @details
#' Temporary files for all the Rmd files and the YAML file which contains these
#' filenames (typically `_bookdown.yml`) are created with modified code chunks.
#' Anywhere in the Rmd files containing [cat()] with knitr-style inline
#' embedded code chunks included are modified to make a string of text and
#' R code together which can be processed by core R. The official knitr
#' regular expression is used to extract these inline code chunks. The main
#' Rmd file, typically `index.Rmd` is not modified at all (it is not parsed).
#'
#' Any single-backslash escaped things in rmarkdown such as `` $\pi$ ``, or
#' `` $\alpha$ `` or similar will be converted to double-backslashed inside
#' of the temporary Rmd files to avoid an error from the [cat()] function.
#'
#' You can use either single or double quotes to surround the text passed
#' to [cat()]. i.e. `cat('contents')` or `cat("contents")`
#'
#' @param yaml_fn The Bookdown YAML file name. '_bookdown.yml' by default
#' @param keep_files If `TRUE`, keep the temporary files created by the
#' pre-processor (`tmp-*.Rmd` and `tmp_bookdown.yml`)
#' @param en_chunk_regex A regular expression to match for the chunk
#' name for English chunks. Default for English chunks is that their names
#' end in `-en`. The regular expression for this is `^\\S+-en$`.
#' The `$` means anchor to the end, so `-en` must be at the end. `\\S+`
#' means match one or more non-whitespace characters. Passed to
#' [validate_chunk_headers()]
#' @param fr_chunk_regex A regular expression to match for the chunk
#' name for French chunks. Default for French chunks is that their names
#' end in `-frn`. The regular expression for this is `^\\S+-frn$`.
#' The `$` means anchor to the end, so `-fr` must be at the end. `\\S+`
#' means match one or more non-whitespace characters. Passed to
#' [validate_chunk_headers()]
#' @param suppress_warnings Logical. If `TRUE`, the call to
#' [bookdown::render_book()] from within this function will have warnings
#' suppressed
#' @param verbose Logical. If `TRUE`, print messages
#' @param ... Additional arguments passed to [bookdown::render_book()]
#'
#' @return Nothing
#' @importFrom purrr prepend imap_chr imap
#' @importFrom stringr str_count
#' @importFrom knitr all_patterns
#' @export
render <- function(yaml_fn = "_bookdown.yml",
                   keep_files = FALSE,
                   en_chunk_regex = "^\\S+-en$",
                   fr_chunk_regex = "^\\S+-fr$",
                   suppress_warnings = TRUE,
                   verbose = FALSE,
                   ...){

  cat("\n")

  # Create the temporary YAML and Rmd files and store their names
  tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files(yaml_fn, verbose)
  tmp_yaml_fn <- tmp_yaml_rmd_fns[[1]]
  tmp_rmd_fns <- tmp_yaml_rmd_fns[[2]]
  if(!keep_files){
    on.exit(unlink(unlist(tmp_yaml_rmd_fns), force = TRUE))
  }
  index_fn <- get_index_filename(tmp_yaml_fn, verbose)
  set_render_type(index_fn, "asis")
  # Get the render type (resdoc_pdf, sr_word, etc)
  render_type <- get_render_type(index_fn, verbose)
  doc_format <- gsub("^\\S+_(\\S+)$", "\\1", render_type)
  pdf_or_word <- `if`(doc_format == "pdf", "PDF", "Word")

  # Make sure all YAML entries are present in `index.Rmd`
  check_yaml(render_type, verbose = verbose)

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  # It sets `options(french)` to the value in the file
  set_language_option(index_fn, verbose)

  set_citations(index_fn, verbose)

  # Set up the console Render message
  csas_render_type <- gsub("(.*)_\\S+$", "\\1", render_type)
  if(csas_render_type == "resdoc"){
    csas_render_type <- "Research Document"
  }else if(csas_render_type == "sr"){
    csas_render_type <- "Science Response"
  }else if(csas_render_type == "techreport"){
    csas_render_type <- "Technical Report"
  } else if(csas_render_type == "manureport"){
    csas_render_type <- "Manuscript Report"
  } else{
    csas_render_type <- "CSAS Document" # nocov
  }
  cat("\n")
  notify("Rendering the ", csas_color(csas_render_type), " as a ",
         csas_color(pdf_or_word), " document in ",
         csas_color(`if`(fr(), "French", "English")), " ...\n\n")

  # Process all Rmd files except for the `index_fn` (tmp-index.Rmd)
  fn_process <- tmp_rmd_fns[tmp_rmd_fns != index_fn]

  # Make sure all chunk headers are of the correct language and have
  # `needs_trans` and `results = 'asis'` chunk headers set correctly
  validate_chunk_headers(fn_process,
                         en_chunk_regex = en_chunk_regex,
                         fr_chunk_regex = fr_chunk_regex,
                         verbose)

  # Remove all comments from code chunks in all files that
  # contain `cat()`, `<<chunk-name>>`, or `rmd_file()`,
  # storing the number of lines removed so we can adjust messages
  # later by that many to refer back to original files
  offsets_comm <- remove_comments_from_chunks(fn_process, verbose)

  # Inject the external Rmarkdown code in files referenced by `rmd_file()`
  # into the actual document code in all files (vector `fn_process`)
  if (nrow(offsets_comm)) {
    offsets_rmd <- inject_rmd_files(fn_process, offsets_comm, verbose)

    # The offsets in the files compared to the input file. These can be used to
    # correct messages with line numbers. Note this table has a row for every
    # chunk in the document but there are only values for those
    # that contained `cat()`, `<<chunk-name>>`, or `rmd_file()` as their
    # contents
    offsets <- offsets_comm |>
      mutate(rmd_num = offsets_rmd$post_num)

    # Replace instances of <<chunk-name>> with code from the actual chunk
    # called `chunk-name`. This works project wide, i.e. a chunk can mirror
    # a chunk from a different file as long as both are in `fn_process`.
    # There can be multiple instances of <<chunk-name>> and all will be replaced
    # with the source for that chunk.
    copy_mirror_chunks(fn_process,
                       line_offsets = offsets,
                       verbose = verbose)

    # Run the pre-processor on all the chunks
    preprocess_chunks(fn_process,
                      yaml_fn,
                      line_offsets = offsets,
                      verbose)
  }

  # Inject some more complex code into the temporary version of index.Rmd
  # so the authors don't have to see it in index.Rmd and cannot change it,
  # which would break the document build process.
  inject_bilingual_code(index_fn, render_type, verbose)

  if(verbose){
    notify("Knitting Rmd files and running Pandoc to build the document ...")
  }

  if(suppress_warnings){
    suppressMessages(
      suppressWarnings(
        render_book(index_fn,
                    config_file = tmp_yaml_fn,
                    envir = parent.frame(n = 2L),
                    ...)
      )
    )
  }else{
    suppressMessages(
      render_book(index_fn,
                  config_file = tmp_yaml_fn,
                  envir = parent.frame(n = 2L),
                  ...)
    )
  }
  if(verbose){
    fn <- file.path("_book",
                    paste0(gsub("^(\\S+)_\\S+$", "\\1", render_type),
                           ".",
                           ifelse(doc_format == "pdf", "pdf", "docx")))
    if(file.exists(fn)){
      check_notify("Knitting and Pandoc completed, document built ",
                   "successfully\n")
    }else{
      # nocov start
      bail("The Knitting and Pandoc procedure did not produce ",
           fn_color(fn))
      # nocov end
    }
  }

  # Rename the output files to include 'english' or 'french' so that
  # rendering different language versions does not overwrite the other
  # language version.
  rename_output_files(index_fn, verbose)
  if(!verbose){
    notify("For help debugging your code, render your document like this:\n",
           csas_color("render(verbose = TRUE, keep_files = TRUE)"), "\n\n")
  }

  # Delete the temporary files
  if(!keep_files) {
    map(fn_process, ~{
      unlink(.x, force = TRUE)
    })
    unlink(tmp_yaml_fn)
    unlink(index_fn)
    unlink("*.log")
    unlink("*.up*")
  }

  check_notify("Render completed")
  invisible()
}
