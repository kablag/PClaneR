plate_wells <- function() {
  grid <- expand.grid(row = LETTERS[1:8], column = 1:12,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$position <- sprintf("%s%02d", grid$row, grid$column)
  grid$is_edge <- grid$row %in% c("A", "H") | grid$column %in% c(1L, 12L)
  grid
}

# A column-wise snake keeps neighbouring technical replicates physically close.
well_priority <- function(avoid_edges = TRUE) {
  grid <- plate_wells()
  columns <- lapply(seq_len(12), function(j) {
    rows <- if (j %% 2L) LETTERS[1:8] else rev(LETTERS[1:8])
    sprintf("%s%02d", rows, j)
  })
  order <- unlist(columns, use.names = FALSE)
  if (!isTRUE(avoid_edges)) return(order)
  inner <- grid$position[!grid$is_edge]
  c(order[order %in% inner], order[!order %in% inner])
}

empty_plate <- function() {
  wells <- plate_wells()
  data.frame(
    position = wells$position,
    row = wells$row,
    column = wells$column,
    sample = "",
    kit = "",
    type = "Empty",
    replicate = NA_integer_,
    channels = "",
    stringsAsFactors = FALSE
  )
}

default_kits <- function() {
  data.frame(
    kit = c("SNP-A", "SNP-B"),
    channels = c("FAM/HEX", "FAM/HEX"),
    sample_repeats = c(2L, 2L),
    pko_name = c("ПКО SNP-A", "ПКО SNP-B"),
    pko_repeats = c(2L, 2L),
    oko_name = c("ОКО SNP-A", "ОКО SNP-B"),
    oko_repeats = c(2L, 2L),
    stringsAsFactors = FALSE
  )
}

kit_columns <- names(default_kits())

normalise_kits <- function(x) {
  missing <- setdiff(kit_columns, names(x))
  if (length(missing)) stop("В Excel отсутствуют столбцы: ", paste(missing, collapse = ", "))
  x <- as.data.frame(x[kit_columns], stringsAsFactors = FALSE)
  for (nm in c("sample_repeats", "pko_repeats", "oko_repeats")) {
    x[[nm]] <- suppressWarnings(as.integer(x[[nm]]))
    if (anyNA(x[[nm]]) || any(x[[nm]] < 0L)) stop("Число повторов должно быть целым и неотрицательным")
  }
  x$kit <- trimws(as.character(x$kit))
  if (any(!nzchar(x$kit)) || anyDuplicated(x$kit)) stop("Названия наборов должны быть заполнены и уникальны")
  for (nm in c("channels", "pko_name", "oko_name")) x[[nm]] <- as.character(x[[nm]])
  x
}

parse_samples <- function(text) {
  values <- unlist(strsplit(text %||% "", "[\r\n;,]+"), use.names = FALSE)
  values <- trimws(values)
  unique(values[nzchar(values)])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

make_reactions <- function(samples, kits, capacity = 96L) {
  kits <- normalise_kits(kits)
  samples <- unique(trimws(as.character(samples)))
  samples <- samples[nzchar(samples)]

  controls <- do.call(rbind, lapply(seq_len(nrow(kits)), function(i) {
    k <- kits[i, ]
    parts <- list()
    if (k$pko_repeats > 0L) parts[[length(parts) + 1L]] <- data.frame(
      sample = k$pko_name, kit = k$kit, type = "PCO",
      replicate = seq_len(k$pko_repeats), channels = k$channels)
    if (k$oko_repeats > 0L) parts[[length(parts) + 1L]] <- data.frame(
      sample = k$oko_name, kit = k$kit, type = "NTC",
      replicate = seq_len(k$oko_repeats), channels = k$channels)
    if (!length(parts)) return(NULL)
    do.call(rbind, parts)
  }))
  if (is.null(controls)) controls <- data.frame(sample=character(), kit=character(), type=character(), replicate=integer(), channels=character())

  per_sample <- sum(kits$sample_repeats)
  available <- capacity - nrow(controls)
  if (available < 0L) stop("Одни только контроли занимают больше 96 лунок")
  n_fit <- if (per_sample == 0L) length(samples) else min(length(samples), floor(available / per_sample))
  placed_samples <- head(samples, n_fit)
  sample_rxns <- do.call(rbind, lapply(placed_samples, function(s) {
    do.call(rbind, lapply(seq_len(nrow(kits)), function(i) {
      k <- kits[i, ]
      if (k$sample_repeats == 0L) return(NULL)
      data.frame(sample = s, kit = k$kit, type = "Sample",
                 replicate = seq_len(k$sample_repeats), channels = k$channels)
    }))
  }))
  if (is.null(sample_rxns)) sample_rxns <- controls[0, ]
  list(reactions = rbind(controls, sample_rxns), placed = placed_samples,
       omitted = setdiff(samples, placed_samples), controls = nrow(controls))
}

build_plate <- function(samples, kits, avoid_edges = TRUE) {
  plan <- make_reactions(samples, kits)
  result <- empty_plate()
  rxns <- plan$reactions
  if (nrow(rxns)) {
    positions <- head(well_priority(avoid_edges), nrow(rxns))
    idx <- match(positions, result$position)
    result[idx, c("sample", "kit", "type", "replicate", "channels")] <- rxns
  }
  attr(result, "plan") <- plan[c("placed", "omitted", "controls")]
  result
}

plate_for_widget <- function(plate) {
  type_labels <- c(Sample = "Образец", PCO = "ПКО", NTC = "ОКО", Empty = "Пусто")
  colors <- c(Sample = "#dbeafe", PCO = "#dcfce7", NTC = "#fee2e2", Empty = "#f8fafc")
  # shinyMolBio follows RDML column names internally, even when custom
  # templates are used.
  plate$target <- plate$kit
  plate$target.dyeId <- plate$channels
  plate$fdata.name <- plate$sample
  plate$react.id <- seq_len(nrow(plate))
  plate$type_label <- unname(type_labels[plate$type])
  plate$label <- ifelse(plate$type == "Empty", "", paste0(plate$sample, "<br><small>", plate$kit, "</small>"))
  plate$style <- paste0("background-color:", unname(colors[plate$type]), ";")
  plate
}
