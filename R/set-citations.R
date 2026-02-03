#' Set the 'other' language citation up so that page 2 has the correct other
#' language based on the value of `fr()`
#'
#' @param fn The bookdown index filename, typically index.Rmd. This file
#' must have a YAML option called 'french' set to either 'true' or 'false'
#' @param render_type The render type (e.g., "resdoc_pdf", "resdoc-b_pdf"). 
#' If NULL, citation checking is skipped for non-bilingual documents.
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @export
set_citations <- function(fn = get_index_filename(
  system.file("rmarkdown",
              "templates",
              "resdoc", # All types have the same index filename
              "skeleton",
              "_bookdown.yml",
              package = "csasdowntex")),
  render_type = NULL,
  verbose = FALSE){

  if(verbose){
    notify("Checking file ", fn_color(fn), " for citations ...")
  }

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist")
  }

  rmd <- readLines(fn)
  ca_pat <- "^citation_english: *(.*)$"
  cf_pat <- "^citation_french: *(.*)$"
  ca_ind <- grep(ca_pat, rmd)
  cf_ind <- grep(cf_pat, rmd)

  # Only check for bilingual citations if this is a bilingual document type
  is_bilingual <- !is.null(render_type) && grepl("-b", render_type)
  
  if(is_bilingual){
    if(fr()){
      if(!length(ca_ind)){
        message("You are missing the `citation_english:` tag in your YAML file:\n",
                fn, ". This citation is required when you build in French as it ",
                "is set to the other language citation on page 2 of ",
                "the document")
        return(invisible())
      }
    }else{
      if(!length(cf_ind)){
        message("You are missing the `citation_french:` tag in your YAML file:\n",
                fn, ". This citation is required when you build in English as it ",
                "is set to the other language citation on page 2 of ",
                "the document")
        return(invisible())
      }
    }
  }
  # Only update citation_other_language for bilingual documents that have both citation types
  if(is_bilingual && length(ca_ind) > 0 && length(cf_ind) > 0){
    ca <- rmd[ca_ind]
    ca <- gsub(ca_pat, "\\1", ca)
    cf <- rmd[cf_ind]
    cf <- gsub(cf_pat, "\\1", cf)

    cother_ind <- grep("^citation_other_language: *", rmd)
    cother <- paste0('citation_other_language: ',
                     ifelse(fr(),
                            ca,
                            cf))

    if(length(cother_ind)){
      rmd[cother_ind] <- cother
    }else{
      end_citations_ind <- max(ca_ind, cf_ind)
      prev <- rmd[1:end_citations_ind]
      last <- rmd[(end_citations_ind + 1):length(rmd)]
      rmd <- c(prev,
               cother,
               last)
    }

    writeLines(rmd, fn)
  }
}
