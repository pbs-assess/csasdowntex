# csasdowntex

# 0.3.0

* The original csasdown was renamed csasdowntex and a new package that focuses
  on CSAS-compliant .docx output is now available as csasdown. csasdowntex
  can continue to be used to reproduce old reports or for Tech Report output
  but CSAS will no longer access PDF Research Documents or Science Responses.

# csasdowntex 0.2.3

* Fix DOI links below ISBN. Previously there was were `{}`
  characters within the URL.

# csasdowntex 0.2.2

* Add new implementation of FSAR by @PaulRegular. #287

* Alias render_fsar() to render_sar().

* Check if using render() on an FSAR by accident.

* Add `config_file` argument to `render_sar()`. #293

# csasdowntex 0.2.1

* Fix DOI in tech reports #288

# csasdowntex 0.2.0

* Compatibility with 2025 Res Doc and SR templates.

# csasdowntex 0.1.8

* Revert some recent commits so the main branch works.

# csasdowntex 0.1.7

* Merge in in-progress fsar template.

# csasdowntex 0.1.6

* Switch back from UKenglish to english for Babel in resdoc English .sty

# csasdowntex 0.1.5

* Updates for compatibility with pandoc >= 3.1.8. #255 #253

* Fix bug with extra . in French sections. #254

# csasdowntex 0.1.4

* Add *optional* `show_continued_text` logical YAML argument to enable or
  disable "Continued..." text on long tables that span multiple pages.

# csasdowntex 0.1.3

* Add `on.exit()` to `csasdowntex::render()` to avoid leaving behind `tmp-` files
  if an R error is encountered.
  
* Fix `csasdowntex::render()` render environment.

# csasdowntex 0.1.2

* The Science Response template now uses `french_title` instead of `title_other`
  for consistency with other formats and should be built with 
  `csasdowntex::render()`. #241

* Add option `create_rstudio_file` in `draft()` to avoid creating an RStudio 
  project file. Stop creating a `.here` file. #243

# csasdowntex 0.1.0

* All messages, warnings, and errors now appear in color if your console supports ANSI color escapes. RStudio does support this and all testing for this feature was done in RStudio 2022.02.3 Build 492.

* `verbose` was added to many functions. If you run `render(verbose = TRUE)` you will see a detailed listing of the steps taken during rendering, including line numbers of important items.

* A new function `csasdowntex::render()` will auto-detect your document type (*4resdoc*, *resdoc-b*, *sr*, *techreport*) and render accordingly. We recommend using this over `bookdown::render_book()`. The 'Knit' button in RStudio should properly choose `csasdowntex::render()`.

* New YAML options are required for this render method; an error will be produced when you run `render()` explaining which ones you are missing. These new YAML options are new French options and are required to render the document. Even if you don't plan on using French, you must enter them with some (any) default text. They are:
   - `french_month`
   - `french_region`
   - `french_address`
   - `french_title`
   - `french_abstract`
   
* The four example documents (*resdoc*, *resdoc-b*, *sr*, *techreport*) contain all the current YAML options.

* There is a new example document called **resdoc-b** which you can get by running `csasdowntex::draft("resdoc-b")` and then rendering with `csasdowntex::render()`. That document is built with and explains all the new features and the new `render()` function.

* If you're using an old version of **index.Rmd** in your project you should modify the `knit:` YAML tag to the following:
  ```
  knit: (function(input, ...) {
        csasdowntex::render('_bookdown.yml')`
        })
  ```
* If you're using an old version of **index.Rmd** in your project you should delete these chunks of code from it:
  ```
  meta <- rmarkdown::metadata$output
  if (length(grep("pdf", names(meta)))) {
    french <- meta$`csasdowntex::resdoc_pdf`$french
    prepub <- meta$`csasdowntex::resdoc_pdf`$prepub
  } else if (length(grep("word", names(meta)))) {
    french <- meta$`csasdowntex::resdoc_word`$french
    prepub <- meta$`csasdowntex::resdoc_word`$prepub
  }
  csl <- "csl/csas.csl"
  if (french) {
    csl <- "csl/csas-french.csl"
    options(OutDec = ",")
  }
  ```
  and
  ```
  ---
  csl: `r csl`    
  ---
  ```

# csasdowntex 0.0.10.9000

* Reduce space above title in SRs #207

* Implement 2022 CSAS formatting updates #204

* Add ISBN and Cat number to Res Docs (new mandatory fields)

* Add `gsub(" :", "~:", x)` to catch dangling `:` in French

# csasdowntex 0.0.8.9001

* Started recording NEWS.md

* Added `run_pdflatex()` for cases where latexmk doesn't run pdflatex enough times to update page numbers for long table of contents. #151

* Decreased spacing in Res Doc abstracts based on CSAP request. #147

* Added Tech Report cover and 2nd pages to match new format exactly. This requires modifying a .docx file. #146

# csasdowntex 0.0.0.9000

* Adapted huskydown for CSAS Res Docs
