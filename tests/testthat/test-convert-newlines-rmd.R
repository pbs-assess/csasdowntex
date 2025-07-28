test_that("convert_newlines_rmd works", {

  str <- c("")
  str_mod <- c("", "\\n", "")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  str <- c("Hello world",
           "",
           "",
           "Welcome!")
  str_mod <- c("Hello world",
               "\\n",
               "",
               "Welcome!")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("#Section header",
           "",
           "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
           "internos aut similique vitae. In sapiente molestias ut aperiam ",
           "autem ut galisum dignissimos.",
           "",
           "",
           "",
           "",
           "#Section header 2")
  str_mod <- c("#Section header",
               "\\n",
               "",
               "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
               "\\\\",
               "",
               "internos aut similique vitae. In sapiente molestias ut aperiam ",
               "\\\\",
               "",
               "autem ut galisum dignissimos.",
               "\\n",
               "",
               "\\n",
               "",
               "\\n",
               "",
               "#Section header 2")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "")
  str_mod <- c("\\n",
               "",
               "\\n",
               "",
               "Lorem ipsum dolor sit amet",
               "\\n",
               "")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("xx",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "",
           "yy")
  str_mod <- c("xx",
               "\\n",
               "",
               "Lorem ipsum dolor sit amet",
               "\\n",
               "",
               "yy")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("",
           "Creating a list",
           "1. Item 1",
           "2. Item 2",
           "   a. Item 2a",
           "   b. Item 2b",
           "      i. Item 2bi",
           "      ii. Item 2bii",
           "3. Item 3.",
           "",
           "End of list")
  str_mod <- c("\\n",
               "",
               "Creating a list",
               "",
               "1. Item 1",
               "2. Item 2",
               "   a. Item 2a",
               "   b. Item 2b",
               "      i. Item 2bi",
               "      ii. Item 2bii",
               "",
               "3. Item 3.",
               "\\n",
               "",
               "End of list")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
           "tempor incididunt ut labore et dolore magna aliqua. Bibendum ut",
           "",
           "",
           "   parameter           value",
           "---------------- -----------------",
           "       a                 5",
           "       b                 8",
           "       c                 9",
           "",
           "----------------------------------------------------------------------------------",
           "  Factors                    Correlation between Parents & Child      Inherited",
           "------------------------- ----------------------------------------- --------------",
           "  Education                                -0.49                         Yes",
           "",
           "",
           "",
           "Socio-Economic Status                     0.28                        Slight",
           "",
           "Income                                    0.08                          No",
           "",
           "Family Size                               0.18                        Slight",
           "",
           "Occupational Prestige                     0.21                        Slight",
           "------------------------- ----------------------------------------- --------------",
           "   Table: Correlation of Inheritance Factors for Parents and Child")

  str_mod <- c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
               "\\\\",
               "",
               "tempor incididunt ut labore et dolore magna aliqua. Bibendum ut",
               "\\n",
               "",
               "   parameter           value",
               "---------------- -----------------",
               "       a                 5",
               "       b                 8",
               "       c                 9",
               "\\n",
               "",
               "----------------------------------------------------------------------------------",
               "  Factors                    Correlation between Parents & Child      Inherited",
               "------------------------- ----------------------------------------- --------------",
               "  Education                                -0.49                         Yes",
               "",
               "Socio-Economic Status                     0.28                        Slight",
               "",
               "Income                                    0.08                          No",
               "",
               "Family Size                               0.18                        Slight",
               "",
               "Occupational Prestige                     0.21                        Slight",
               "------------------------- ----------------------------------------- --------------",
               "   Table: Correlation of Inheritance Factors for Parents and Child")

  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("-----",
           "  a",
           "-----",
           "  b",
           "-----",
           "",
           "other text",
           "",
           "-----",
           "  x",
           "-----",
           "  y",
           "",
           "",
           "",
           "-----",
           "Table: 2nd Caption")

  str_mod <- c("-----",
               "  a",
               "-----",
               "  b",
               "-----",
               "\\n",
               "",
               "other text",
               "\\n",
               "",
               "-----",
               "  x",
               "-----",
               "  y",
               "-----",
               "Table: 2nd Caption")
  ret <- convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

})