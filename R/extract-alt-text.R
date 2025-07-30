#' Extract contents of text found in a `(ref:variable-alt)` found in any rmd
#' file
#'
#' @details
#' 1. Starts by parsing the `_bookdown.yml` file to extract the names of all
#'    uncommented rmd files in the current build.
#' 2. Searches all those files for any lines starting with `(ref:variable-alt)`.
#'    Preceding spaces are ignored.
#' 3. Extracts all the text found for the label given by `inp_str`, and
#'    returns the description found for it (the actual alternative text)
#' 4. If no name is found, the code will figure out what the figure number
#'    is (including preceding appendix letters) and set that as the alternative
#'    text
#'
#' Originally implemented in the `hake` package but copied here for simplicity
#' so that the hake package did not need to be imported as that is a different
#' system of building than csasdown
#'
#' @param inp_str The string to match, in the format `(ref:alt-text-label)`
#' @param bookdown_fn The '_bookdown.yml' file to use, typically in the root
#' @param bookdown_fn The '_bookdown.yml' file to use, typically in the root
#' directory given by [here::here()]
#'
#' @return The text found for the label given by `inp_str`
extract_alt_text <- function(inp_str,
                             bookdown_fn = "_bookdown.yml"){

  if(!file.exists(bookdown_fn)){
    bail("When attempting to extract alt text, file '",
         bookdown_fn, "' does not exist.")
  }
  bd_lines <- readLines(bookdown_fn)
  bd_rmd_raw <- grep("\\.[R|r]md", bd_lines, value = TRUE)
  # Remove leading and trailing whitespace
  bd <- trimws(bd_rmd_raw)
  # Remove commented-out lines (for speed)
  if(length(grep("^#", bd))){
    bd <- bd[-grep("^#", bd)]
  }
  bd <- bd |>
    # Remove escaped quotes
    stringr::str_remove_all('\"') %>%
    # Remove `rmd_files: [`
    gsub("^rmd_files: \\[", "", .) %>%
    # Remove closing `]` and commas
    gsub("\\]$", "", .) %>%
    gsub(",$", "", .)

  fns <- bd
  if(fr()){
    alt_str <- paste0(inp_str, "-fr-alt")
  }else{
    alt_str <- paste0(inp_str, "-en-alt")
  }
  alt_str_ref <- paste0("^ *\\(ref:", alt_str, "\\)")

  k <- map(fns, ~{
    rmd <- readLines(.x)
    x <- grep(alt_str_ref, rmd)

    if(length(x)){
      if(length(x) > 1){
        bail("Alt. text label `", alt_str, "` defined more than once in ",
             "file `", .x, "`")
      }
      # Find all lines that belong in the alt text (there may be newlines
      # in between them in the source rmd file). Assuming that after the
      # alt text is done, it will be followed by either a blank line or
      # the start of a chunk (starts with ```), or the end-of-file
      start_ind <- x
      end_ind <- x
      repeat{
        # Check the line for a new chunk or a blank line
        is_chunk <- grep("```", rmd[end_ind]) |>
          length() |>
          as.logical()
        is_blank_line <-  grep("^$", rmd[end_ind]) |>
          length() |>
          as.logical()
        is_eof <<- end_ind == length(rmd)
        if(is_chunk || is_blank_line || is_eof){
          break
        }
        end_ind <- end_ind + 1
      }
      # Now on either chunk start of blank line, so remove that line from
      # the text, checking the EOF conditions
      if(is_eof){
        # Only need to check if there's a blank line. A chunk cannot start
        # (and end) on the same line so no need to check that at EOF
        if(!length(grep("^$", rmd[length(rmd)]))){
          end_ind <- end_ind - 1
        }
      }else{
        end_ind <- end_ind - 1
      }

      # Glue all the text lines together
      alt_text <- paste(rmd[start_ind:end_ind], collapse = " ")

      # Remove the label
      ref_regex <- paste0("\\(ref:", alt_str, "\\) *")
      alt_text <- gsub(ref_regex, "", alt_text)

      # Check for percent signs and replace with the word percent. Alt text
      # cannot have percent signs in un-escaped and the escape character shows
      # up in the output text and will be read by a reader so we remove percent
      # signs (if any) and all preceding backslashes and spaces (if any)
      #
      # First, remove all slashes before percent signs
      alt_text <- gsub("([\\]+%)", "%", alt_text)
      # If there is a format argument for a printf-type R statement inside
      # R chunk(s), the percent sign will be followed by a letter or number,
      # if so leave the percent sign intact be so that the R code will execute
      # properly
      alt_text <- gsub("%(?![a-zA-Z0-9]+)",
                       ifelse(fr(),
                              " pour cent",
                              " percent"),
                       alt_text,
                       perl = TRUE)

      # Return a vector of the label and it's text
      alt_text
    }else{
      NULL
    }
  })

  # Remove all NULLs from the list
  k[sapply(k, is.null)] <- NULL

  if(length(k) == 1){

    k <- unlist(k)

    # Replace any inline r code with actual text (mini-knitr parser)
    # Break up the string into chunks before and after the inline r code chunks
    # TEST
    # sp <- "hake"
    # common_name <- "this is the common name"
    # k <- "`r sp` `r sp` - Trying to match `r sp` with another `r 10 + 29 * 30` chunk `r common_name`."

    backtick_inds <- unlist(gregexpr("`", k))
    if(backtick_inds[1] == -1){
      backtick_inds <- NULL
    }

    # Remove inline R code chunks prior to checking for commas in alt text
    # so that commas in the function calls do not match
    tmp <- gsub("`r.*`", "", k)
    # Check for commas that haven't been escaped. They will cause tagpdf errors
    # if not fixed. (?<!\\\\) is negative lookbehind which means that if a comma
    # is not preceded by a double backslash, then replace the comma with \\,
    # Note that for lookbehinds/lookaheads to work we must use perl = TRUE
    comma_pat <- "(?<!\\\\)(\\,)"

    grep_length <- length(grep(comma_pat, tmp, perl = TRUE))
    if(grep_length){
      if(grep_length == 1){
        message <- "There was a comma "
      }else{
        message <- paste0("There were ", grep_length, " commas ")
      }
      alert(message, "found in the alt text entry for label `",
            alt_str, "` You must re-write the alternative text without commas. ",
            "This is a limitation of the tagpdf LaTeX package.\nFor nowe, the ",
            "comma was removed so the compilation could continue.\n The text in ",
            "question is:\n\n",
            k, "\n")
      # Remove the commas - was used when this was a warning (alert) instead, but
      # the warning does not print to the screen so it is useless.
      k <- gsub(comma_pat, "", k, perl = TRUE)
      # Replace with double-backslash. Note this leaves a backslash in the text
      # and there's no way around it. I tried more or less backslashes and no dice
      #k <- gsub(comma_pat, "\\\\\\1", k, perl = TRUE)
    }

    if(!length(backtick_inds)){
      return(k)
    }
    if(length(backtick_inds) %% 2 != 0){
      bail("There is an odd number of backticks in the text referred ",
           "to by label ", inp_str, ". The text is:\n", k)
    }

    chunks_non_r <- str_split(k, "`r .*?`")[[1]]
    chunks_non_r <- chunks_non_r[chunks_non_r != ""]

    # Number of backticks are even as they must be, so break them into chunks
    start_inds <- backtick_inds[seq(1, length(backtick_inds), 2)]
    # Check to make sure the starting backticks have an 'r' immediately after
    walk(start_inds, ~{
      if(substr(k, .x + 1, .x + 1) != "r" && substr(k, .x + 2, .x + 2) != " "){
        stop("Non-r code chunk found. R code chunks must be of the format ",
             "`r code_here`")
      }
    })
    end_inds <- backtick_inds[seq(2, length(backtick_inds), 2)]
    chunks <- str_sub(k, start_inds, end_inds)

    # Evaluate the R chunks. This is needed so that inline R chunks found in
    # alt text paragraphs are evaluated properly
    chunks <- map_chr(chunks, ~{
      # Remove `r and ` from the code
      x <- gsub("^`r", "", .x)
      x <- gsub("`$", "", x)
      x <- gsub(" +", "", x)
      eval(parse(text = x))
    })
    # Here we have chunks and chunks_non_r. We need to find out which comes fist,
    chunk_len <- max(length(chunks), length(chunks_non_r))
    length(chunks) <- chunk_len
    length(chunks_non_r) <- chunk_len

    if(start_inds[1] == 1){
      out_str <- c(rbind(chunks, chunks_non_r))
    }else{
      out_str <- c(rbind(chunks_non_r, chunks))
    }
    out_str <- out_str[!is.na(out_str)]
    out_str <- paste(out_str, collapse = "")

  }else if(!length(k)){
    # Create a Figure XX alt text label. Need to check which appendix
    # it is in or if it is in the main document figures
    rmd <- map(fns, ~{
      readLines(.x)
    }) |>
      unlist()
    fig_chunk_inds <- grep("fig.cap *=", rmd)
    fig_chunk_code <- rmd[fig_chunk_inds]
    pat <- paste0(inp_str, " *,")
    the_fig_ind <- grep(pat, fig_chunk_code)
    if(!length(the_fig_ind)){
      bail("There was a problem matching the name of the figure chunk '",
           inp_str, "' using the regular expression '", pat, "'.\nThere ",
           "was no match. This could occur if the name is not ",
           "followed by zero or more spaces and then a comma.")
    }
    if(length(the_fig_ind) > 1){
      bail("There was a problem matching the name of the figure chunk '",
           inp_str, "' using the regular expression '", pat, "'.\nThere ",
           "was more than one match. This could occur if the name is not ",
           "followed by zero or more spaces and then a comma.")
    }
    # Check if in an appendix and if so, which one
    is_appendix_fig <- FALSE
    appendix_ind <- grep("Appendix \\{-\\}", rmd)
    if(length(appendix_ind) == 1){
      is_appendix_fig <- fig_chunk_inds[the_fig_ind] > appendix_ind
      # Which appendix is it in?
      if(is_appendix_fig){
        # Search for all heading 1 lines following the appendix declaration
        appendix_lines <- rmd[appendix_ind:length(rmd)]
        #pattern <- "(?<![#])# +(?![#])"
        # Assumes that the author has put {#app:<letter>} into the source code
        pattern <- ".*(\\{#app:[a-z]\\}) *$"
        # This will match both English and French appendix header lines.
        # We use the French ones as guides
        app_headers <- grep(pattern, appendix_lines, perl = TRUE, value = TRUE)
        if(!length(app_headers)){
          bail("Could not find any tags on the appendix headers of the ",
               "format: {#app:<letter>} where <letter> is a lower-case letter ",
               "from a to z. This is required for automated alternative text ",
               "to work. Here is an example line for an Appendix header:\n",
               "# Ecosystem considerations {#app:d}")
        }
        # Remove all but the last occurrence of app:<letter> if there are more
        # than one
        doubles <- gsub(pattern, "\\1", app_headers)
        app_headers <- app_headers[duplicated(doubles)]
        app_header_patterns <- gsub("\\{", "\\\\{", app_headers)
        app_header_patterns <- gsub("\\}", "\\\\}", app_header_patterns)
        app_header_patterns <- gsub("\\(", "\\\\(", app_header_patterns)
        app_header_patterns <- gsub("\\)", "\\\\)", app_header_patterns)
        app_header_inds <- map_dbl(app_header_patterns, ~{grep(.x, rmd)})

        # Find out which appendix the figure is in
        which_app <- max(which(fig_chunk_inds[the_fig_ind] > app_header_inds))
        which_app_letter <- LETTERS[which_app]
        # Now find which figure number in the appendix it is 1-100 ?!
        # There is more than one appendix, so we have to look in the one we
        # are in and count up the figures in it
        last_app <- length(app_header_inds)
        if(which_app == last_app){
          # It is the last appendix, so we search to the end of the document
          app_lines <- rmd[app_header_inds[which_app]:length(rmd)]
        }else{
          # There must be at least one appendix following this one
          app_lines <- rmd[app_header_inds[which_app]:app_header_inds[which_app + 1]]
        }
        fig_chunk_inds <- grep("fig.cap *=", app_lines)
        fig_chunk_code <- app_lines[fig_chunk_inds]
        pat <- paste0(inp_str, " *,")
        the_fig_ind <- grep(pat, fig_chunk_code)
      }

    }else if(length(appendix_ind) > 1){
      bail("More than one 'Appendix {-}' was found while trying to objectify ",
           "alternative text figure numbers. Check Rmd code")
    }

    if(is_appendix_fig){
      out_str <- paste0("Figure ", which_app_letter, ".", the_fig_ind)
    }else{
      out_str <- paste0("Figure ", the_fig_ind)
    }

  }else{
    bail("Error retrieving your alternative text label (ref:", alt_str, "). ",
         "There was more than one found in the code.")
  }


  out_str
}