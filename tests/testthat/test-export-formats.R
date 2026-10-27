source(testthat::test_path("..", "..", "R", "plate_logic.R"))
source(testthat::test_path("..", "..", "R", "export_formats.R"))

testthat::test_that("Innova maps samples and controls to instrument types", {
  plate <- build_plate(c("S1", "S2"), default_kits()[1, ], FALSE, 1, 1, 1,
                       "horizontal", "continuous", "adjacent")
  file <- tempfile(fileext=".xlsx")
  write_innova_xlsx(plate, file, testthat::test_path("..", "..", "data", "innova.xlsx"))
  x <- openxlsx::read.xlsx(file, sheet="Sample Set", colNames=FALSE,
                           skipEmptyRows=FALSE, skipEmptyCols=FALSE)
  testthat::expect_true(all(c("UN", "PC", "NC") %in% x[[4]]))
  expected_names <- plate$sample[plate$type != "Empty"]
  testthat::expect_true(all(expected_names %in% x[[5]]))
  testthat::expect_false(any(c("ПКО 1", "ПКО 2", "ПКО 3", "к- ПЦР") %in% x[[5]]))
  testthat::expect_equal(x[[3]][3], "A1")
})

testthat::test_that("Bio-Rad uses documented sample type names", {
  plate <- build_plate("S1", default_kits()[1, ], FALSE, 1, 1, 1,
                       "horizontal", "continuous", "adjacent")
  file <- tempfile(fileext=".csv")
  write_biorad_csv(plate, file)
  x <- utils::read.csv(file, check.names=FALSE, fileEncoding="UTF-8")
  testthat::expect_equal(names(x), c("Row", "Column", "Sample Type", "Replicate #",
                                     "*Target Name", "*Sample Name", "Starting Quantity", "Units"))
  testthat::expect_true(all(c("Unknown", "Positive Control", "NTC") %in% x[["Sample Type"]]))
  testthat::expect_equal(x[["*Sample Name"]][1], "S1")
})

testthat::test_that("Bio-Rad export also supports an empty plate", {
  file <- tempfile(fileext=".csv")
  testthat::expect_silent(write_biorad_csv(empty_plate(), file))
  x <- utils::read.csv(file, check.names=FALSE)
  testthat::expect_equal(nrow(x), 0)
})
