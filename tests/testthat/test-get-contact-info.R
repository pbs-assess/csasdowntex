test_that("get_contact_info() works", {

  expect_warning(get_contact_info(NULL),
                 "Region not detected; using national CSAS contact info")
  expect_warning(get_contact_info(NA),
                 "Region not detected; using national CSAS contact info")
  expect_warning(get_contact_info(""),
                 "Region not detected; using national CSAS contact info")

  # ---------------------------------------------------------------------------
  options(french = FALSE)
  region <- "nonexistent"
  expect_warning(r <- get_contact_info(region),
                 "Region not detected; using national CSAS contact info")
  expect_identical(r$email, "csas-sccs@dfo-mpo.gc.ca")
  expect_identical(r$address, "200 Kent St.\\\\\\\\Ottawa, ON\\\\enspace K1A 0E6")

  # ---------------------------------------------------------------------------
  options(french = TRUE)
  region <- "nonexistent"
  expect_warning(r <- get_contact_info(region),
                 "Region not detected; using national CSAS contact info")
  expect_identical(r$email, "csas-sccs@dfo-mpo.gc.ca")
  expect_identical(r$address, "200 Kent St.\\\\\\\\Ottawa (ON) K1A 0E6")

  # ---------------------------------------------------------------------------
  # English doc, English region
  options(french = FALSE)
  region <- "Pacific Region"
  r <- get_contact_info(region)
  expect_identical(r$email, "DFO.PacificCSA-CASPacifique.MPO@dfo-mpo.gc.ca")
  expect_identical(r$address, "3190 Hammond Bay Rd.\\\\\\\\Nanaimo, BC\\\\enspace V9T 6N7")

  # ---------------------------------------------------------------------------
  # English doc, accidental French region
  options(french = FALSE)
  region <- "Région du Pacifique"
  r <- get_contact_info(region)
  expect_identical(r$email, "DFO.PacificCSA-CASPacifique.MPO@dfo-mpo.gc.ca")
  expect_identical(r$address, "3190 Hammond Bay Rd.\\\\\\\\Nanaimo, BC\\\\enspace V9T 6N7")

  # ---------------------------------------------------------------------------
  # French doc, French region
  options(french = TRUE)
  region <- "Région du Pacifique"
  r <- get_contact_info(region)
  expect_identical(r$email, "DFO.PacificCSA-CASPacifique.MPO@dfo-mpo.gc.ca")
  expect_identical(r$address, "3190, chemin Hammond Bay\\\\\\\\Nanaimo (C.-B.) V9T 6N7")

  # ---------------------------------------------------------------------------
  # French doc, accidental English region
  options(french = TRUE)
  region <- "Pacific Region"
  r <- get_contact_info(region)
  expect_identical(r$email, "DFO.PacificCSA-CASPacifique.MPO@dfo-mpo.gc.ca")
  expect_identical(r$address, "3190, chemin Hammond Bay\\\\\\\\Nanaimo (C.-B.) V9T 6N7")


  options(french = FALSE)
})
