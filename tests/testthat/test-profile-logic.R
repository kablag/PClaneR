source(testthat::test_path("..", "..", "R", "plate_logic.R"))
source(testthat::test_path("..", "..", "R", "profile_logic.R"))

testthat::test_that("profiles are read from workbook sheets and retain temperatures", {
  file <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(
    "Профиль A" = data.frame(`Краткое название` = c("A1", "A2"), Та = c("60оС", "63оС"), Каналы = c("FAM/HEX", "HEX/ROX"), check.names = FALSE),
    "Профиль B" = data.frame(`Краткое название` = "A1", Та = "60оС", Каналы = "FAM/HEX", check.names = FALSE)
  ), file)
  profiles <- read_profiles(file)
  testthat::expect_equal(names(profiles), c("Профиль A", "Профиль B"))
  testthat::expect_equal(profiles[["Профиль A"]]$kit, c("A1", "A2"))
  testthat::expect_equal(profiles[["Профиль A"]]$annealing_temp, c("60оС", "63оС"))
  testthat::expect_equal(profile_kits(profiles[["Профиль A"]], "60оС")$kit, "A1")
})

testthat::test_that("series creates the next plate when samples do not fit", {
  kits <- default_kits()[1, ]
  samples <- paste0("S", seq_len(100))
  series <- build_plate_series(samples, kits, sample_repeats = 1, pko_repeats = 1, oko_repeats = 1)
  testthat::expect_gt(length(series), 1)
  testthat::expect_true(all(vapply(series, function(x) sum(x$type != "Empty") <= 96L, logical(1))))
  actual <- unique(unlist(lapply(series, function(x) x$sample[x$type == "Sample"])))
  testthat::expect_setequal(actual, samples)
})

testthat::test_that("by-kit profiles are split into separate plate kit groups", {
  kits <- default_kits()[rep(1, 10), ]
  kits$kit <- paste0("K", seq_len(nrow(kits)))
  for (column in c("pko_wt_name", "pko_h_name", "pko_m_name", "oko_name")) {
    kits[[column]] <- paste(column, kits$kit)
  }
  groups <- split_kits_by_lanes(kits, "horizontal", TRUE)
  testthat::expect_equal(length(groups), 2L)
  testthat::expect_equal(unname(vapply(groups, nrow, integer(1))), c(6L, 4L))
  testthat::expect_silent(build_plate("S1", groups[[1]], TRUE, 1, 1, 1, "horizontal", "by_kit", "mirrored"))
})

testthat::test_that("kit partition keeps all samples of a kit together where possible", {
  kits <- default_kits()[rep(1, 10), ]
  kits$kit <- paste0("K", seq_len(nrow(kits)))
  for (column in c("pko_wt_name", "pko_h_name", "pko_m_name", "oko_name")) {
    kits[[column]] <- paste(column, kits$kit)
  }
  samples <- paste0("S", seq_len(6))
  groups <- partition_kits_for_full_samples(samples, kits, TRUE, 1, 1, 1,
                                            "horizontal", "by_kit", "mirrored")
  testthat::expect_gt(length(groups), 1L)
  for (group in groups) {
    plate <- build_plate(samples, group, TRUE, 1, 1, 1, "horizontal", "by_kit", "mirrored")
    testthat::expect_length(attr(plate, "plan")$omitted, 0L)
  }
})

testthat::test_that("controls can be placed at the beginning of a kit block", {
  plate <- build_plate(c("S1", "S2"), default_kits()[1, ], TRUE, 1, 1, 1,
                       "horizontal", "by_kit", "adjacent", "beginning")
  used <- plate[plate$type != "Empty", ]
  testthat::expect_equal(used$type[1], "PCO")
  testthat::expect_equal(used$type[nrow(used)], "Sample")
})
