str_vec <- c(
  "Testing $Box$",
  "Testing $hat{alpha}$, and $xi$",
  "Testing $hat{O}$, and $theta$",
  "Testing $eta + zeta$, and $theta$",
  "Testing $eta+zeta$, and $theta$",
  "Testing $eta - zeta$, and $theta$",
  "Testing $eta * zeta$, and $theta$",
  "Testing $eta / zeta$, and $theta$",
  "Testing $eta^zeta$, and $theta$",
  "Testing $\\Box$",
  "Testing $\\hat{alpha}$, and $\\\\xi$",
  "Testing $hat{\\\\O}$, and $\\\\\\\\theta$",
  "Testing $ddot{\\Digamma}/bar{omicron}$",
  "Testing $\\\\\\ddot{\\Digamma}/bar{\\\\omicron}$",
  "Testing $breve{E}$, and $varphi^2+sin{theta}$"
)

test_that("csasdown::escape_latex_symbols() function works", {

  i <- map_chr(str_vec, ~{
    csasdown:::escape_latex_symbols(.x)
  })

  expect_equal(i[1], "Testing $\\Box$")
  expect_equal(i[2], "Testing $\\hat{\\alpha}$, and $\\xi$")
  expect_equal(i[3], "Testing $\\hat{\\O}$, and $\\theta$")
  expect_equal(i[4], "Testing $\\eta + \\zeta$, and $\\theta$")
  expect_equal(i[5], "Testing $\\eta+\\zeta$, and $\\theta$")
  expect_equal(i[6], "Testing $\\eta - \\zeta$, and $\\theta$")
  expect_equal(i[7], "Testing $\\eta * \\zeta$, and $\\theta$")
  expect_equal(i[8], "Testing $\\eta / \\zeta$, and $\\theta$")
  expect_equal(i[9], "Testing $\\eta^\\zeta$, and $\\theta$")
  expect_equal(i[10], "Testing $\\Box$")
  expect_equal(i[11], "Testing $\\hat{\\alpha}$, and $\\xi$")
  expect_equal(i[12], "Testing $\\hat{\\O}$, and $\\theta$")
  expect_equal(i[13], "Testing $\\ddot{\\Digamma}/\\bar{\\omicron}$")
  expect_equal(i[14], "Testing $\\ddot{\\Digamma}/\\bar{\\omicron}$")
  expect_equal(i[15], "Testing $\\breve{\\E}$, and $\\varphi^2+\\sin{\\theta}$")

})
