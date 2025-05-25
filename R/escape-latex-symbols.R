#' Search in a string for LaTeX symbols and ensure they have two backslashes before them
#'
#' @param s A string
#'
#' @returns The original string `str` but with two backslashes preceding all
#' LaTeX symbols found in the symbols list in the function (`gr`)
#'
#' @export
escape_latex_symbols <- function(str){

  # Following list is from several places but official source is the
  # "Comprehensive LaTeX Symbol List" found here:
  # http://tug.ctan.org/info/symbols/comprehensive/symbols-a4.pdf
  #
  # The following is from:
  # https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
  # except for hat which was added separately
  gr <- c(# Greek letters
          "alpha", "A", "beta", "B", "gamma", "Gamma", "delta", "Delta",
          "epsilon", "varepsilon", "E", "zeta", "Z", "eta", "H", "theta",
          "Theta", "vartheta", "iota", "I", "kappa", "K",  "lambda", "Lambda",
          "mu", "M", "nu", "N", "xi","Xi", "o", "O", "omicron", "Omicron",
          "pi", "Pi", "rho", "varrho", "P", "sigma", "Sigma", "tau", "T",
          "upsilon", "Upsilon", "phi", "varphi", "chi", "X", "psi", "Psi",
          "omega", "Omega",
          # Over lines etc from CLSL page 136
          "acute", "bar", "breve", "check", "dot", "ddot", "dddot", "ddddot",
          "grave", "hat", "mathring", "middlebar", "middleslash",  "overline",
          "strokethrough", "tilde", "vec",
          # Archaic
          "digamma", "Digamma",
          # Arrows
          "leftarrow", "Leftarrow", "rightarrow", "Rightarrow", "leftrightarrow",
          "rightleftharpoons", "uparrow", "downarrow", "Uparrow", "Downarrow",
          "Leftrightarrow", "Updownarrow", "mapsto", "longmapsto", "nearrow",
          "searrow", "swarrow", "nwarrow", "leftharpoonup", "rightharpoonup",
          "leftharpoondown", "rightharpoondown",
          # Miscellaneous symbols
          "infty", "forall", "Re", "Im", "nabla", "exists", "partial", "nexists",
          "emptyset", "varnothing", "wp", "complement", "neg", "cdots", "square",
          "surd", "blacksquare", "triangle",
          # Binary operations and relations
          "times", "cdot", "div", "cap", "cup", "leq", "geq", "in", "perp",
          "subset", "simeq", "wedge", "vee", "Box", "boxtimes",
          # Relation operators from
          # https://oeis.org/wiki/List_of_LaTeX_mathematical_symbols
          "nless", "ngtr", "leqslant", "geqslant", "nleq", "ngeq", "nleqslant",
          "ngeqslant", "prec", "succ", "nprec", "nsucc", "preceq", "succeq",
          "npreceq", "nsucceq", "ll", "gg", "lll", "ggg", "subset", "supset",
          #"not\\subset", "not\\supset", # Strange syntax so ignore
          "subseteq", "supseteq", "nsubseteq", "nsupseteq", "sqsubset", "sqsupset",
          "sqsubseteq", "sqsupseteq", "doteq", "equiv", "approx", "cong",
          "simeq", "sim", "propto", "neq", "ne", "parallel", "nparallel",
          "asymp", "bowtie", "vdash", "dashv", "in", "ni", "smile", "frown",
          "models", "notin", "mid",
          # Binary operators from
          # https://oeis.org/wiki/List_of_LaTeX_mathematical_symbols
          "pm", "mp", "times", "ast", "dagger", "ddagger", "uplus", "sqcap",
          "sqcup", "diamond", "bigtriangleup", "bigtriangledown", "trinagleleft",
          "trinagleright", "bigcirc", "bullet", "wr", "oplus", "ominus",
          "otimes", "oslash", "odot", "circ", "setminus", "amalg",
          # Logic operators from above
          "lor", "land", "implies", "Longrightarrow", "Longleftarrow", "top",
          "bot",
          # Geometry
          "angle", "overrightarrow", "measuredangle", "ncong", "nsim",
          #"not\\perp # weird syntax, ignore for now
          # Delimiters from above
          "lceil", "ulcorner", "rceil", "urcorner", "lfloor", "llcorner",
          "backslash", "langle", "rangle", "rfloor", "lrcorner",
          # Other
          "eth", "hbar", "imath", "jmath", "ell", "beth", "gimel",
          # Trig
          "sin", "cos", "tan", "arcsin", "arccos", "arctan", "csc", "sec",
          "cot", "arccsc", "arcsec", "arccot",
          "sinh", "cosh", "tanh", "arcsinh", "arccosh", "arctanh", "csch", "sech",
          "coth", "arccsch", "arcsech", "arccoth"
  )

  # Remove all instances of more than two backslashes
  str <- gsub("\\\\+", "\\\\", str)

  # Get math chunks
  math_chunks <- str_extract_all(str, "\\$.*?\\$")[[1]]
  if(length(math_chunks)){
    math_chunks_fixed <- map(math_chunks, \(chunk){
      walk(gr, \(symbol){
        # if(symbol == "zeta" || symbol == "eta")
        #   browser()
        chunk <<- gsub(paste0("(?<![\\\\|a-zA-Z0-9])(", symbol, ")(?![a-zA-Z0-9])"),
             "\\\\\\1",
             chunk,
             perl = TRUE)
        #message(chunk, "\n", symbol, "\n\n")
      })
      chunk
    })
  }

  str_out <- str
  # Replace the chunks back where they were in the input string
  walk2(math_chunks, math_chunks_fixed, \(chunk, chunk_fixed){
    # The `str_replace()` application strips two backslashes off so we
    # replace them first
    chunk_fixed <- gsub("\\\\", "\\\\\\\\", chunk_fixed)
    # Need to turn the extracted original chunk into a regular expression
    # for the replacement function which means escaping all the special
    # characters
    chunk <- gsub("\\\\", "\\\\\\\\", chunk)
    chunk <- gsub("\\$", "\\\\$", chunk)
    chunk <- gsub("\\{", "\\\\{", chunk)
    chunk <- gsub("\\}", "\\\\}", chunk)
    chunk <- gsub("\\+", "\\\\+", chunk)
    chunk <- gsub("\\-", "\\\\-", chunk)
    chunk <- gsub("\\*", "\\\\*", chunk)
    chunk <- gsub("\\/", "\\\\/", chunk)
    chunk <- gsub("\\^", "\\\\^", chunk)
    str_out <<- str_replace(str_out, chunk, chunk_fixed)
  })

  str_out
}
