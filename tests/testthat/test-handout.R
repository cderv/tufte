test_that("tufte_handout() does not warn about crop tools", {
  expect_no_warning(tufte_handout())
})

test_that("tufte_book() does not warn about crop tools", {
  expect_no_warning(tufte_book())
})

test_that("fig_crop = 'auto' disables crop hook when tools are missing", {
  # Crop-tool detection (pdfcrop + ghostscript) varies by platform and
  # CRAN check environment, so query rmarkdown's own resolver to decide
  # whether the preconditions for this test hold. If rmarkdown would
  # itself set a crop hook for fig_crop = "auto", the test's premise
  # does not apply on this machine.
  baseline <- rmarkdown::knitr_options_pdf(4, 2.5, "auto", "pdf")
  skip_if(
    !is.null(baseline$knit_hooks$crop),
    "crop tools must be missing for this test"
  )
  fmt <- tufte_handout()
  expect_null(fmt$knitr$knit_hooks$crop)
})

test_that("tufte_handout renders without pdfcrop warnings", {
  skip_on_cran()
  skip_if_not_pandoc()
  rmd <- local_rmd_file(c(
    "---",
    "title: test",
    "output: tufte::tufte_handout",
    "---",
    "",
    "Hello world."
  ))
  # Suppress unrelated LaTeX warnings, but fail on any crop-related ones
  withCallingHandlers(
    local_render(rmd),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("pdfcrop|crop_tools|ghostscript", msg)) {
        fail(paste("Unexpected crop tool warning:", msg))
      }
      invokeRestart("muffleWarning")
    }
  )
  succeed()
})
