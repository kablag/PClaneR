instrument_well_order <- function() {
  as.vector(t(outer(LETTERS[1:8], 1:12, function(row, column) paste0(row, column))))
}

ordered_plate <- function(plate) {
  positions <- instrument_well_order()
  plate[match(positions, sub("0([1-9])$", "\\1", plate$position)), , drop = FALSE]
}

innova_sample_id <- function(sample, type) {
  result <- sample
  result[type == "PCO" & grepl("_wt", sample, ignore.case = TRUE)] <- "ПКО 1"
  result[type == "PCO" & grepl("_h", sample, ignore.case = TRUE)] <- "ПКО 2"
  result[type == "PCO" & grepl("_m", sample, ignore.case = TRUE)] <- "ПКО 3"
  # Fallback for custom positive-control names without wt/h/m suffixes.
  other_pco <- which(type == "PCO" & !grepl("_wt|_h|_m", sample, ignore.case = TRUE))
  if (length(other_pco)) result[other_pco] <- sample[other_pco]
  result[type == "NTC"] <- "к- ПЦР"
  result
}

write_innova_xlsx <- function(plate, file, template = file.path("data", "innova.xlsx")) {
  if (!file.exists(template)) stop("Не найден шаблон Innova: ", template)
  x <- ordered_plate(plate)
  sample_type <- unname(c(Sample = "UN", PCO = "PC", NTC = "NC", Empty = NA_character_)[x$type])
  sample_id <- ifelse(x$type == "Empty", NA_character_, innova_sample_id(x$sample, x$type))
  values <- data.frame(
    No = seq_len(96),
    Well = sub("0([1-9])$", "\\1", x$position),
    Sample.Type = sample_type,
    Sample.ID = sample_id,
    stringsAsFactors = FALSE
  )
  wb <- openxlsx::loadWorkbook(template)
  openxlsx::writeData(wb, "Sample Set", matrix(NA, nrow=96, ncol=4),
                      startRow=3, startCol=2, colNames=FALSE, rowNames=FALSE, keepNA=FALSE)
  openxlsx::writeData(wb, "Sample Set", values, startRow=3, startCol=2,
                      colNames=FALSE, rowNames=FALSE, keepNA=FALSE)
  openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
}

biorad_sample_type <- function(type) {
  unname(c(Sample = "Unknown", PCO = "Positive Control", NTC = "NTC")[type])
}

write_biorad_csv <- function(plate, file) {
  x <- ordered_plate(plate)
  x <- x[x$type != "Empty", , drop = FALSE]
  result <- data.frame(
    Row = x$row,
    Column = x$column,
    `Sample Type` = biorad_sample_type(x$type),
    `Replicate #` = x$replicate,
    `*Target Name` = x$kit,
    `*Sample Name` = x$sample,
    `Starting Quantity` = rep("", nrow(x)),
    Units = rep("", nrow(x)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  utils::write.table(result, file, sep=",", row.names=FALSE, col.names=TRUE,
                     quote=TRUE, na="", fileEncoding="UTF-8")
}

write_internal_xlsx <- function(plate, kits, file) {
  occupied <- plate[plate$type != "Empty", ]
  occupied <- occupied[order(occupied$layout_order, na.last=TRUE), ]
  occupied <- occupied[c("position", "sample", "kit", "type", "replicate", "channels")]
  matrix_view <- matrix("", nrow=8, ncol=12, dimnames=list(LETTERS[1:8], 1:12))
  for (i in seq_len(nrow(plate))) {
    matrix_view[plate$row[i], as.character(plate$column[i])] <-
      if (plate$type[i] == "Empty") "" else paste(plate$sample[i], plate$kit[i], sep=" | ")
  }
  openxlsx::write.xlsx(list(Plate=occupied, Map=as.data.frame(matrix_view), Kits=kits), file, overwrite=TRUE)
}
