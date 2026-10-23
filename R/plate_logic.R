plate_wells <- function() {
  grid <- expand.grid(row = LETTERS[1:8], column = 1:12,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$position <- sprintf("%s%02d", grid$row, grid$column)
  grid$is_edge <- grid$row %in% c("A", "H") | grid$column %in% c(1L, 12L)
  grid
}

# A column-wise snake keeps neighbouring technical replicates physically close.
mirror_order <- function(x) {
  if (length(x) < 2L) return(x)
  result <- x
  result[seq.int(1L, length(x), by = 2L)] <- head(x, ceiling(length(x) / 2))
  result[seq.int(2L, length(x), by = 2L)] <- rev(tail(x, floor(length(x) / 2)))
  result
}

well_priority <- function(avoid_edges = TRUE, orientation = c("vertical", "horizontal"),
                          balance = c("adjacent", "mirrored"), required_wells = 0L) {
  orientation <- match.arg(orientation)
  balance <- match.arg(balance)
  unlist(lane_positions(orientation, avoid_edges, balance, required_wells), use.names = FALSE)
}

lane_positions <- function(orientation = c("horizontal", "vertical"), avoid_edges = TRUE,
                           balance = c("adjacent", "mirrored"), required_wells = 0L) {
  orientation <- match.arg(orientation)
  balance <- match.arg(balance)
  required_wells <- as.integer(required_wells)
  if (orientation == "horizontal") {
    if (avoid_edges && required_wells <= 60L) {
      lane_names <- LETTERS[2:7]
      columns <- 2:11
    } else if (avoid_edges && required_wells <= 72L) {
      lane_names <- LETTERS[2:7]
      columns <- 1:12
    } else if (avoid_edges) {
      # When all eight rows are needed, physical A:H order prevents a kit
      # block from wrapping across the plate boundary.
      lane_names <- LETTERS[1:8]
      columns <- 1:12
    } else {
      lane_names <- LETTERS[1:8]
      columns <- 1:12
    }
    if (balance == "mirrored") lane_names <- mirror_order(lane_names)
    lapply(lane_names, function(row) sprintf("%s%02d", row, columns))
  } else {
    columns <- if (avoid_edges && required_wells <= 80L) 2:11 else 1:12
    if (balance == "mirrored") columns <- mirror_order(columns)
    lapply(columns, function(column) sprintf("%s%02d", LETTERS[1:8], column))
  }
}

positions_by_kit <- function(reactions, orientation, avoid_edges, balance) {
  # Lanes inside a kit are always physically adjacent. In mirrored mode whole
  # kit blocks alternate between the two sides of the usable plate area.
  lanes <- lane_positions(orientation, avoid_edges, "adjacent", nrow(reactions))
  positions <- character()
  kit_names <- unique(reactions$kit)
  primary_count <- length(lanes)
  low <- 1L
  high <- primary_count
  overflow <- primary_count + 1L

  for (kit_number in seq_along(kit_names)) {
    kit_name <- kit_names[kit_number]
    kit_reactions <- reactions[reactions$kit == kit_name, , drop = FALSE]
    block <- ifelse(kit_reactions$type == "Sample",
                    paste("sample", kit_reactions$sample, sep = ":"),
                    paste("control", kit_reactions$sample, sep = ":"))
    blocks <- split(seq_len(nrow(kit_reactions)), factor(block, levels = unique(block)))

    capacity <- length(lanes[[1]])
    lanes_needed <- 1L
    remaining_capacity <- capacity
    for (block_rows in blocks) {
      size <- length(block_rows)
      if (size > remaining_capacity && size <= capacity) {
        lanes_needed <- lanes_needed + 1L
        remaining_capacity <- capacity
      }
      while (size > 0L) {
        take <- min(size, remaining_capacity)
        size <- size - take
        remaining_capacity <- remaining_capacity - take
        if (size > 0L) {
          lanes_needed <- lanes_needed + 1L
          remaining_capacity <- capacity
        }
      }
    }

    if (balance == "mirrored" && high - low + 1L >= lanes_needed) {
      if (kit_number %% 2L == 1L) {
        selected_indices <- low:(low + lanes_needed - 1L)
        low <- low + lanes_needed
      } else {
        selected_indices <- (high - lanes_needed + 1L):high
        high <- high - lanes_needed
      }
    } else if (balance == "adjacent" && high - low + 1L >= lanes_needed) {
      selected_indices <- low:(low + lanes_needed - 1L)
      low <- low + lanes_needed
    } else {
      if (overflow + lanes_needed - 1L > length(lanes)) {
        stop("Раскладка «по наборам» не помещается: выберите сплошное заполнение или другое направление")
      }
      selected_indices <- overflow:(overflow + lanes_needed - 1L)
      overflow <- overflow + lanes_needed
    }

    kit_lanes <- lanes[selected_indices]
    lane_index <- 1L
    offset <- 0L
    for (block_rows in blocks) {
      remaining_block <- length(block_rows)
      while (remaining_block > 0L) {
        if (lane_index > length(kit_lanes)) {
          stop("Раскладка «по наборам» не помещается: выберите сплошное заполнение или другое направление")
        }
        free <- length(kit_lanes[[lane_index]]) - offset
        if (remaining_block > free && remaining_block <= length(kit_lanes[[lane_index]])) {
          lane_index <- lane_index + 1L
          offset <- 0L
          next
        }
        take <- min(remaining_block, free)
        if (take > 0L) {
          positions <- c(positions, kit_lanes[[lane_index]][offset + seq_len(take)])
          offset <- offset + take
          remaining_block <- remaining_block - take
        }
        if (offset == length(kit_lanes[[lane_index]])) {
          lane_index <- lane_index + 1L
          offset <- 0L
        }
      }
    }
  }
  positions
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
    layout_order = NA_integer_,
    layout_slot = NA_integer_,
    layout_kit = "",
    stringsAsFactors = FALSE
  )
}

default_kits <- function() {
  data.frame(
    kit = c("SNP-A", "SNP-B"),
    channels = c("FAM/HEX", "FAM/HEX"),
    pko_wt_name = c("ПКО_wt SNP-A", "ПКО_wt SNP-B"),
    pko_h_name = c("ПКО_h SNP-A", "ПКО_h SNP-B"),
    pko_m_name = c("ПКО_m SNP-A", "ПКО_m SNP-B"),
    oko_name = c("ОКО SNP-A", "ОКО SNP-B"),
    stringsAsFactors = FALSE
  )
}

kit_columns <- names(default_kits())

normalise_kits <- function(x) {
  # Migrate workbooks created by the first application version.
  if (!"pko_wt_name" %in% names(x) && "pko_name" %in% names(x)) {
    x$pko_wt_name <- sub("^ПКО", "ПКО_wt", as.character(x$pko_name))
  }
  if (!"pko_h_name" %in% names(x)) x$pko_h_name <- paste("ПКО_h", x$kit)
  if (!"pko_m_name" %in% names(x)) x$pko_m_name <- paste("ПКО_m", x$kit)
  missing <- setdiff(kit_columns, names(x))
  if (length(missing)) stop("В Excel отсутствуют столбцы: ", paste(missing, collapse = ", "))
  x <- as.data.frame(x[kit_columns], stringsAsFactors = FALSE)
  x$kit <- trimws(as.character(x$kit))
  if (any(!nzchar(x$kit)) || anyDuplicated(x$kit)) stop("Названия наборов должны быть заполнены и уникальны")
  for (nm in c("channels", "pko_wt_name", "pko_h_name", "pko_m_name", "oko_name")) {
    x[[nm]] <- trimws(as.character(x[[nm]]))
  }
  x
}

parse_samples <- function(text) {
  values <- unlist(strsplit(text %||% "", "[\r\n;,]+"), use.names = FALSE)
  values <- trimws(values)
  unique(values[nzchar(values)])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

make_reactions <- function(samples, kits, sample_repeats = 2L,
                           pko_repeats = 1L, oko_repeats = 1L,
                           capacity = 96L) {
  kits <- normalise_kits(kits)
  repeats <- as.integer(c(sample_repeats, pko_repeats, oko_repeats))
  if (anyNA(repeats) || any(repeats < 0L)) stop("Число повторов должно быть целым и неотрицательным")
  sample_repeats <- repeats[1]; pko_repeats <- repeats[2]; oko_repeats <- repeats[3]
  if (sample_repeats < 1L) stop("Для образцов должен быть задан хотя бы один повтор")
  samples <- unique(trimws(as.character(samples)))
  samples <- samples[nzchar(samples)]

  controls <- do.call(rbind, lapply(seq_len(nrow(kits)), function(i) {
    k <- kits[i, ]
    parts <- list()
    if (pko_repeats > 0L) for (control in c("pko_wt_name", "pko_h_name", "pko_m_name")) {
      parts[[length(parts) + 1L]] <- data.frame(
        sample = k[[control]], kit = k$kit, type = "PCO",
        replicate = seq_len(pko_repeats), channels = k$channels)
    }
    if (oko_repeats > 0L) parts[[length(parts) + 1L]] <- data.frame(
      sample = k$oko_name, kit = k$kit, type = "NTC",
      replicate = seq_len(oko_repeats), channels = k$channels)
    if (!length(parts)) return(NULL)
    do.call(rbind, parts)
  }))
  if (is.null(controls)) controls <- data.frame(sample=character(), kit=character(), type=character(), replicate=integer(), channels=character())

  per_sample <- nrow(kits) * sample_repeats
  available <- capacity - nrow(controls)
  if (available < 0L) stop("Одни только контроли занимают больше 96 лунок")
  n_fit <- if (per_sample == 0L) length(samples) else min(length(samples), floor(available / per_sample))
  placed_samples <- head(samples, n_fit)
  sample_rxns <- do.call(rbind, lapply(placed_samples, function(s) {
    do.call(rbind, lapply(seq_len(nrow(kits)), function(i) {
      k <- kits[i, ]
      if (sample_repeats == 0L) return(NULL)
      data.frame(sample = s, kit = k$kit, type = "Sample",
                 replicate = seq_len(sample_repeats), channels = k$channels)
    }))
  }))
  if (is.null(sample_rxns)) sample_rxns <- controls[0, ]
  list(reactions = rbind(sample_rxns, controls), placed = placed_samples,
       omitted = setdiff(samples, placed_samples), controls = nrow(controls))
}

build_plate <- function(samples, kits, avoid_edges = TRUE, sample_repeats = 2L,
                        pko_repeats = 1L, oko_repeats = 1L,
                        orientation = c("horizontal", "vertical"),
                        grouping = c("by_kit", "continuous"),
                        balance = c("mirrored", "adjacent")) {
  orientation <- match.arg(orientation)
  grouping <- match.arg(grouping)
  balance <- match.arg(balance)
  plan <- make_reactions(samples, kits, sample_repeats, pko_repeats, oko_repeats)
  result <- empty_plate()
  rxns <- plan$reactions
  if (nrow(rxns)) {
    # Both modes keep every kit, including its controls, as one reaction block.
    rxns <- rxns[order(match(rxns$kit, kits$kit)), , drop = FALSE]
    if (grouping == "by_kit") {
      positions <- positions_by_kit(rxns, orientation, avoid_edges, balance)
    } else {
      positions <- head(well_priority(avoid_edges, orientation, "adjacent", nrow(rxns)), nrow(rxns))
    }
    if (length(positions) < nrow(rxns)) stop("На 96-луночной плашке недостаточно места для всех реакций")
    idx <- match(positions, result$position)
    result[idx, c("sample", "kit", "type", "replicate", "channels")] <- rxns
    result$layout_order[idx] <- seq_len(nrow(rxns))
    result$layout_slot[idx] <- seq_len(nrow(rxns))
    result$layout_kit[idx] <- rxns$kit
  }
  actual_samples <- unique(result$sample[result$type == "Sample"])
  if (!setequal(actual_samples, plan$placed)) {
    stop("Внутренняя ошибка раскладки: не все запланированные образцы перенесены на плашку")
  }
  attr(result, "plan") <- plan[c("placed", "omitted", "controls")]
  result
}

compact_plate <- function(plate, remove_positions, preserve_kits = FALSE) {
  remove_idx <- match(remove_positions, plate$position, nomatch = 0L)
  remove_idx <- remove_idx[remove_idx > 0L & plate$type[remove_idx] != "Empty"]
  if (!length(remove_idx)) return(plate)

  content <- c("sample", "kit", "type", "replicate", "channels")
  plate[remove_idx, c("sample", "kit", "channels")] <- ""
  plate$type[remove_idx] <- "Empty"
  plate$replicate[remove_idx] <- NA_integer_
  plate$layout_order[remove_idx] <- NA_integer_

  groups <- if (isTRUE(preserve_kits)) unique(plate$layout_kit[nzchar(plate$layout_kit)]) else "__all__"
  next_order <- 1L
  for (group in groups) {
    slot_idx <- if (group == "__all__") which(!is.na(plate$layout_slot)) else which(plate$layout_kit == group)
    slot_idx <- slot_idx[order(plate$layout_slot[slot_idx])]
    remaining <- plate[plate$type != "Empty" & if (group == "__all__") TRUE else plate$kit == group, , drop = FALSE]
    remaining <- remaining[order(remaining$layout_order, na.last = TRUE), , drop = FALSE]
    if (nrow(remaining) > length(slot_idx)) stop("Недостаточно слотов для уплотнения плашки")

    plate[slot_idx, c("sample", "kit", "channels")] <- ""
    plate$type[slot_idx] <- "Empty"
    plate$replicate[slot_idx] <- NA_integer_
    plate$layout_order[slot_idx] <- NA_integer_
    if (nrow(remaining)) {
      target <- head(slot_idx, nrow(remaining))
      plate[target, content] <- remaining[content]
      plate$layout_order[target] <- seq.int(next_order, length.out = nrow(remaining))
      next_order <- next_order + nrow(remaining)
    }
  }
  plate
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
