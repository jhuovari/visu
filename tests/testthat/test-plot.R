test_that("viiva paksunee karkeimmasta siloitetuimpaan", {
  expect_equal(visu:::visu_linewidths(1), 0.8)
  expect_equal(visu:::visu_linewidths(2), c(0.45, 0.8))
  expect_equal(visu:::visu_linewidths(3), c(0.3, 0.55, 0.8))

  nelja <- visu:::visu_linewidths(4)
  expect_length(nelja, 4L)
  expect_false(is.unsorted(nelja))
  expect_equal(nelja[4], 0.8)
})

test_that("luokkien maara luetaan tekijan tasoista, ei riveista", {
  d <- data.frame(sarja = factor(c("a", "a", "b"), levels = c("a", "b", "c")))

  # Kayttamaton taso "c" ei saa varata omaa viivanpaksuutta.
  expect_equal(visu:::visu_level_count(d, "sarja"), 2L)
})

test_that("linewidth kartoitetaan ja saa oman skaalan", {
  d <- data.frame(
    time = rep(as.Date(c("2024-01-01", "2024-02-01")), 2),
    sarja = factor(rep(c("Alkuperäinen", "Trendi"), each = 2),
                   levels = c("Alkuperäinen", "Trendi")),
    values = c(1, 2, 1.5, 1.6)
  )

  p <- visu_plot(d, linewidth = "sarja")

  expect_true("linewidth" %in% names(p$mapping))
  expect_equal(p$scales$get_scales("linewidth")$palette(2), c(0.45, 0.8))
})

test_that("linewidth ei sovi pylvaille eika alueille", {
  d <- data.frame(time = as.Date("2024-01-01"), sarja = "a", values = 1)

  expect_error(visu_plot(d, linewidth = "sarja", type = "col"), "line")
  expect_error(visu_plot(d, linewidth = "sarja", type = "area"), "line")
})

test_that("tuntematon sarake kaataa selvalla viestilla", {
  d <- data.frame(time = as.Date("2024-01-01"), values = 1)

  expect_error(visu_plot(d, linewidth = "puuttuu"), "linewidth")
})
