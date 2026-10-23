source(testthat::test_path("..", "..", "R", "plate_logic.R"))

testthat::test_that("96-well grid is complete and unique", {
  x <- plate_wells()
  testthat::expect_equal(nrow(x), 96)
  testthat::expect_equal(length(unique(x$position)), 96)
  testthat::expect_equal(sum(!x$is_edge), 60)
})

testthat::test_that("inner wells are used before edges", {
  p <- well_priority(TRUE)
  grid <- plate_wells()
  testthat::expect_true(all(p[1:60] %in% grid$position[!grid$is_edge]))
})

testthat::test_that("only complete samples are placed", {
  kits <- default_kits()
  kits$sample_repeats <- 20L
  kits$pko_repeats <- 2L
  kits$oko_repeats <- 2L
  result <- build_plate(paste0("S", 1:10), kits)
  info <- attr(result, "plan")
  testthat::expect_equal(length(info$placed), 2)
  testthat::expect_equal(sum(result$type != "Empty"), 88)
  counts <- table(result$sample[result$type == "Sample"])
  testthat::expect_true(all(counts == 40))
})
