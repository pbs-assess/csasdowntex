#' Set a YAML tag to a value inside the YAML header in the file given
#'
#' @keywords internal
#'
#' @param tag A regular expression representing the name of the tag. Usually
#' just the name of the tag is enough but if it occurs more than once in the
#' file, you will need to refine your regular expression here
#'
#' @param val The new value to set `tag` to. Can be a vector of character
#' strings, in which case they will each be placed on their own line,
#' separated properly by YAML syntax. Do not include the OR bar (`|`) or
#' any newline characters, they will be added by the function
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @importFrom tibble enframe tibble
#' @importFrom purrr map2 map2_lgl
#' @return Nothing
set_yaml_tag <- function(tag = NULL, val = NULL, fn = "index.Rmd"){

  if(is.null(fn)){
    bail("Filename ", csas_color("fn"), " cannot be ", csas_color("NULL"))
  }

  if(is.null(tag)){
    bail("YAML tag ", tag_color("tag"), " cannot be ", csas_color("NULL"))
  }

  if(is.null(val)){
    bail("YAML tag value ", tag_color("val"), " cannot be ", csas_color("NULL"))
  }

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist")
  }

  rmd <- readLines(fn)

  if(!length(rmd)){
    bail("File ", fn_color(fn), " does not contain anything")
  }
  #rmd <- c(rmd, "---", "approver:", "", "kfeujbvk:  kmneec", "---")

  if(length(grep("csasdown::", tag)) || length(grep("output", tag))){
    bail("The ", tag_color("output:"), " and ", tag_color("csasdown::<doc_type>"),
         " tags cannot be changed with this function.\n",
         "Use ", csas_color("csasdown:::set_render_type()"), " instead")
  }

  if(length(grep("knit", tag))){
    bail("The ", tag_color("knit:"), " tag cannot be modified")
  }

  # ---------------------------------------------------------------------------
  # Parse the file, finding start and end of YAML blocks, which are
  # surrounded by ---. There may be multiple blocks, so use a stack parser
  stk <- NULL
  start_yaml_inds <- NULL
  end_yaml_inds <- NULL
  i <- 1
  repeat{
    if(length(grep("^---\\s*$", rmd[i]))){
      if(stk_size(stk)){
        tmp <- stk_pop(stk)
        stk <- tmp[[1]]
        end_yaml_inds <- c(end_yaml_inds, i)
      }else{
        stk <- stk_push(stk, "---")
        start_yaml_inds <- c(start_yaml_inds, i)
      }
    }
    if(i == length(rmd)){
      break
    }
    i <- i + 1
  }
  # start_yaml_inds and end_yaml_inds hold the indices of the --- lines
  if(length(start_yaml_inds) != length(end_yaml_inds)){
    bail("There are uneven sets of ", tag_color("---"),
         " lines meaning an unending YAML ",
         "block in file ", fn_color(fn))
  }
  if(!length(start_yaml_inds)){
    alert("There were no YAML blocks found in the file. Nothing was changed")
    return(invisible())
  }

  # ---------------------------------------------------------------------------
  # Set up the values and find where the tag is in the file
  if(length(val) > 1){
    val <- paste("|\n ", paste(val, collapse = "\\ \n  "))
  }
  # which inds are inside a YAML block. Must start with the tag or spaces
  tag <- paste0("^\\s*", tag)
  inds <- grep(tag, rmd)
  inds <- map2(start_yaml_inds, end_yaml_inds, ~{
    if(.x + 1 == .y){
      return(NULL) # nocov
    }
    inds[inds %in% (.x + 1):(.y - 1)]
  }) |>
    unlist()

  if(!length(inds)){
    bail("The YAML tag ", tag_color(tag), " was not found in the file ",
         fn_color(fn))
  }

  if(length(inds) > 1){
    bail("The YAML tag ", tag_color(tag), " was found more than once ",
         "in the file ", fn_color(fn), ".\nRefine your regular expression ",
         "for ", csas_color("tag"), " and try again.",
         "\nThe matching lines (", csas_color(paste(inds, collapse = ", ")),
         ") are:\n\n",
         csas_color(paste(rmd[inds], collapse = "\n")))
  }

  # ---------------------------------------------------------------------------
  # Guaranteed to have only one tag location in the file at this point
  ind <- inds
  # Which YAML block is the tag in?
  which_block <- imap(start_yaml_inds, ~{
    if(ind > start_yaml_inds[.y] && ind < end_yaml_inds[.y]){
      return(.y)
    }
  }) |>
    unlist()

  tag_line <- rmd[ind]
  tag_name <- gsub("^\\s*([A-Za-z0-9_\\-]+):(\\s*\\S+\\s*)+$", "\\1", tag_line)

  # If the tag is more than one line, need to remove the old lines from the file
  # and replace with the new ones. Extra lines will be indented by two spaces or
  # more relative to the tag indentation
  lead_spc_tag <- gsub("^(\\s*).*$", "\\1", rmd[ind])
  i <- ind + 1
  repeat{
    lead_spc <- gsub("^(\\s*).*$", "\\1", rmd[i])
    the_rest <- gsub("^\\s*(.*)$", "\\1", rmd[i])
    if(lead_spc == "" || # Not indented at all, so it's not part of the tag
       lead_spc == lead_spc_tag || # Not indented relative to the tag
       i == end_yaml_inds[which_block]){ # End of the YAML block
      i <- i - 1
      break
    }
    i <- i + 1
  }

  # Extract leading spaces from the tag line to place in the new line
  leading_spaces <- gsub("^(\\s*).*$", "\\1", tag_line)
  tag_output <- paste0(leading_spaces, tag_name, ": ", val)
  rmd <- c(rmd[1:(ind - 1)], tag_output, rmd[(i + 1):length(rmd)])

  writeLines(rmd, fn)
}