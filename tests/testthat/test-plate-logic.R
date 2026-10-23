source(testthat::test_path("..", "..", "R", "plate_logic.R"))

testthat::test_that("96-well grid is complete and unique", {
  x <- plate_wells()
  testthat::expect_equal(nrow(x), 96)
  testthat::expect_equal(length(unique(x$position)), 96)
  testthat::expect_equal(sum(!x$is_edge), 60)
})

testthat::test_that("edge policy depends on orientation", {
  horizontal <- well_priority(TRUE, "horizontal", required_wells=60)
  vertical <- well_priority(TRUE, "vertical", required_wells=80)
  testthat::expect_equal(length(horizontal), 60)
  testthat::expect_true(all(sub("[A-Z]", "", horizontal) %in% sprintf("%02d", 2:11)))
  testthat::expect_equal(length(vertical), 80)
  testthat::expect_true(all(sub("[A-Z]", "", vertical) %in% sprintf("%02d", 2:11)))
})

testthat::test_that("only complete samples are placed", {
  kits <- default_kits()
  result <- build_plate(paste0("S", 1:10), kits, sample_repeats = 20L,
                        pko_repeats = 2L, oko_repeats = 2L,
                        grouping = "continuous")
  info <- attr(result, "plan")
  testthat::expect_equal(length(info$placed), 2)
  testthat::expect_equal(sum(result$type != "Empty"), 96)
  counts <- table(result$sample[result$type == "Sample"])
  testthat::expect_true(all(counts == 40))
})

testthat::test_that("three positive control types are added for every kit", {
  plan <- make_reactions("S1", default_kits(), sample_repeats=1, pko_repeats=2, oko_repeats=1)
  testthat::expect_equal(sum(plan$reactions$type == "PCO"), 12)
  testthat::expect_true(all(c("ПКО_wt SNP-A", "ПКО_h SNP-A", "ПКО_m SNP-A") %in% plan$reactions$sample))
  testthat::expect_equal(plan$reactions$type[1], "Sample")
})

testthat::test_that("sample repetitions cannot be zero", {
  testthat::expect_error(make_reactions("S1", default_kits(), sample_repeats=0),
                         "хотя бы один повтор")
})

testthat::test_that("mirrored lanes alternate opposite sides", {
  horizontal <- lane_positions("horizontal", TRUE, "mirrored")
  vertical <- lane_positions("vertical", TRUE, "mirrored")
  testthat::expect_equal(sub("[0-9]+", "", vapply(horizontal[1:6], `[`, "", 1)),
                         c("B", "G", "C", "F", "D", "E"))
  testthat::expect_equal(as.integer(sub("[A-Z]", "", vapply(vertical[1:6], `[`, "", 1))),
                         c(2, 11, 3, 10, 4, 9))
})

testthat::test_that("a kit continues on the next lane before its controls", {
  result <- build_plate(c("S1", "S2", "S3", "S4", "S5"), default_kits()[1, ],
                        avoid_edges=TRUE, sample_repeats=2, pko_repeats=1,
                        oko_repeats=1, orientation="horizontal",
                        grouping="by_kit", balance="mirrored")
  used <- result[result$type != "Empty", ]
  testthat::expect_equal(used$sample[match("B02", used$position)], "S1")
  testthat::expect_equal(used$sample[match("B11", used$position)], "S5")
  testthat::expect_equal(used$sample[match("C02", used$position)], "ПКО_wt SNP-A")
  testthat::expect_false(any(used$position %in% c("B01", "B12")))
})

testthat::test_that("the next kit starts only after all lanes of the previous kit", {
  result <- build_plate(c("S1", "S2", "S3", "S4", "S5"), default_kits(),
                        avoid_edges=TRUE, sample_repeats=2, pko_repeats=1,
                        oko_repeats=1, orientation="horizontal",
                        grouping="by_kit", balance="mirrored")
  used <- result[result$type != "Empty", ]
  testthat::expect_true(all(used$row[used$kit == "SNP-A"] %in% c("B", "C")))
  testthat::expect_true(all(used$row[used$kit == "SNP-B"] %in% c("F", "G")))
})

testthat::test_that("controls continue sequentially across a lane boundary", {
  result <- build_plate(c("s1", "s2", "s3", "s4"), default_kits()[1, ],
                        avoid_edges=TRUE, sample_repeats=2, pko_repeats=1,
                        oko_repeats=1, orientation="horizontal",
                        grouping="by_kit", balance="mirrored")
  used <- result[result$type != "Empty", ]
  testthat::expect_true(all(sprintf("B%02d", 2:9) %in% used$position[used$type == "Sample"]))
  controls <- used[used$type != "Sample", ]
  testthat::expect_equal(controls$position[match(c("ПКО_wt SNP-A", "ПКО_h SNP-A", "ПКО_m SNP-A", "ОКО SNP-A"), controls$sample)],
                         c("B10", "B11", "C02", "C03"))
})

testthat::test_that("multi-row kit blocks mirror as whole blocks", {
  result <- build_plate(c("s1", "s2", "s3", "s4"), default_kits(),
                        avoid_edges=TRUE, sample_repeats=2, pko_repeats=1,
                        oko_repeats=1, orientation="horizontal",
                        grouping="by_kit", balance="mirrored")
  used <- result[result$type != "Empty", ]
  testthat::expect_true(all(used$row[used$kit == "SNP-A"] %in% c("B", "C")))
  testthat::expect_true(all(used$row[used$kit == "SNP-B"] %in% c("F", "G")))
  controls <- used[used$kit == "SNP-B" & used$type != "Sample", ]
  testthat::expect_equal(controls$position[match(c("ПКО_wt SNP-B", "ПКО_h SNP-B", "ПКО_m SNP-B", "ОКО SNP-B"), controls$sample)],
                         c("F10", "F11", "G02", "G03"))
})

testthat::test_that("horizontal layout enables side columns and restarts at B01", {
  p <- well_priority(TRUE, "horizontal", "adjacent", required_wells=61)
  testthat::expect_equal(length(p), 72)
  testthat::expect_equal(p[1], "B01")
  testthat::expect_equal(p[12], "B12")
})

testthat::test_that("vertical layout uses A:H before enabling side columns", {
  inner <- well_priority(TRUE, "vertical", "adjacent", required_wells=80)
  full <- well_priority(TRUE, "vertical", "adjacent", required_wells=81)
  testthat::expect_equal(inner[1], "A02")
  testthat::expect_equal(inner[8], "H02")
  testthat::expect_equal(full[1], "A01")
})

testthat::test_that("continuous mode keeps each kit and its controls together", {
  result <- build_plate(paste0("s", 1:12), default_kits(), TRUE, 3, 1, 1,
                        "horizontal", "continuous", "mirrored")
  used <- result[result$type != "Empty", ]
  route <- well_priority(TRUE, "horizontal", "adjacent", nrow(used))
  used <- used[match(route[seq_len(nrow(used))], used$position), ]
  testthat::expect_equal(rle(used$kit)$values, c("SNP-A", "SNP-B"))
  testthat::expect_true(all(tail(used$type[used$kit == "SNP-A"], 4) != "Sample"))
  testthat::expect_true(all(tail(used$type[used$kit == "SNP-B"], 4) != "Sample"))
})

testthat::test_that("full horizontal plate never wraps a kit across H to A", {
  for (mode in c("continuous", "by_kit")) {
    result <- build_plate(paste0("s", 1:18), default_kits(), TRUE, 2, 1, 1,
                          "horizontal", mode, "mirrored")
    used <- result[result$type != "Empty", ]
    testthat::expect_true(all(used$row[used$kit == "SNP-A"] %in% LETTERS[1:4]))
    expected_b_rows <- if (mode == "by_kit") LETTERS[5:8] else LETTERS[4:7]
    testthat::expect_true(all(used$row[used$kit == "SNP-B"] %in% expected_b_rows))
    testthat::expect_true(all(c("PCO", "NTC") %in% unique(used$type[used$kit == "SNP-A"])))
    testthat::expect_true(all(c("PCO", "NTC") %in% unique(used$type[used$kit == "SNP-B"])))
  }
})

testthat::test_that("extracting reactions compacts all following slots", {
  original <- build_plate(c("s1", "s2", "s3"), default_kits()[1, ], TRUE, 2, 1, 1,
                          "horizontal", "continuous", "adjacent")
  before <- original[order(original$layout_slot), ]
  before <- before[!is.na(before$layout_slot), ]
  removed <- before$position[c(2, 4)]
  expected_samples <- before$sample[-c(2, 4)]
  compacted <- compact_plate(original, removed)
  after <- compacted[order(compacted$layout_slot), ]
  after <- after[after$type != "Empty", ]
  testthat::expect_equal(after$sample, expected_samples)
  testthat::expect_equal(after$layout_order, seq_len(nrow(after)))
  testthat::expect_true(all(tail(compacted$type[order(compacted$layout_slot)], 2) == "Empty"))
})

testthat::test_that("by-kit extraction never shifts another kit into the gap", {
  original <- build_plate(c("s1", "s2", "s3"), default_kits(), TRUE, 2, 1, 1,
                          "horizontal", "by_kit", "mirrored")
  a_before <- original[original$layout_kit == "SNP-A", ]
  a_before <- a_before[order(a_before$layout_slot), ]
  b_before <- original[original$kit == "SNP-B" & original$type != "Empty", ]
  b_before <- b_before[order(b_before$layout_order), ]
  compacted <- compact_plate(original, a_before$position[2:3], preserve_kits=TRUE)
  b_after <- compacted[compacted$kit == "SNP-B" & compacted$type != "Empty", ]
  b_after <- b_after[order(b_after$layout_order), ]
  testthat::expect_equal(b_after$position, b_before$position)
  testthat::expect_false(any(compacted$kit == "SNP-B" & compacted$layout_kit == "SNP-A"))
})

testthat::test_that("horizontal continuous layout progresses by rows", {
  p <- well_priority(FALSE, "horizontal")
  testthat::expect_equal(p[1:12], sprintf("A%02d", 1:12))
  testthat::expect_equal(p[13:24], sprintf("B%02d", 1:12))
})

testthat::test_that("by-kit horizontal layout starts each kit on a new row", {
  result <- build_plate("S1", default_kits(), avoid_edges=FALSE, sample_repeats=1,
                        pko_repeats=1, oko_repeats=1,
                        orientation="horizontal", grouping="by_kit", balance="adjacent")
  used <- result[result$type != "Empty", ]
  testthat::expect_equal(unique(used$row[used$kit == "SNP-A"]), "A")
  testthat::expect_equal(unique(used$row[used$kit == "SNP-B"]), "B")
  testthat::expect_equal(sum(used$type == "Sample"), 2)
})

testthat::test_that("by-kit vertical layout starts each kit in a new column", {
  result <- build_plate("S1", default_kits(), avoid_edges=FALSE, sample_repeats=1,
                        pko_repeats=1, oko_repeats=1,
                        orientation="vertical", grouping="by_kit", balance="adjacent")
  used <- result[result$type != "Empty", ]
  testthat::expect_equal(unique(used$column[used$kit == "SNP-A"]), 1)
  testthat::expect_equal(unique(used$column[used$kit == "SNP-B"]), 2)
})
