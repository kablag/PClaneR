profile_column <- function(x, candidates, required = TRUE) {
  normalize_name <- function(value) tolower(gsub("[[:space:]_.-]+", "", enc2utf8(value)))
  names_normalized <- normalize_name(names(x))
  wanted <- normalize_name(candidates)
  i <- match(wanted, names_normalized, nomatch = 0L)
  i <- i[i > 0L]
  if (!length(i)) {
    if (required) stop("На листе профиля нет столбца: ", paste(candidates, collapse = " / "))
    return(NULL)
  }
  names(x)[i[1]]
}

normalise_profile_sheet <- function(x, profile) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  kit_col <- profile_column(x, c("Краткое название", "Набор", "kit"))
  temp_col <- profile_column(x, c("Ta", "Та", "Температура отжига", "температура"))
  channels_col <- profile_column(x, c("Каналы", "channels"))
  result <- data.frame(
    profile = profile,
    kit = trimws(as.character(x[[kit_col]])),
    annealing_temp = trimws(as.character(x[[temp_col]])),
    channels = trimws(as.character(x[[channels_col]])),
    stringsAsFactors = FALSE
  )
  result <- result[nzchar(result$kit), , drop = FALSE]
  if (!nrow(result)) stop("В профиле «", profile, "» нет наборов")
  if (any(!nzchar(result$annealing_temp))) stop("В профиле «", profile, "» не заполнена Ta")
  if (anyDuplicated(result$kit)) stop("В профиле «", profile, "» названия наборов должны быть уникальны")
  result$pko_wt_name <- paste("ПКО_wt", result$kit)
  result$pko_h_name <- paste("ПКО_h", result$kit)
  result$pko_m_name <- paste("ПКО_m", result$kit)
  result$oko_name <- paste("ОКО", result$kit)
  result
}

read_profiles <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  if (!length(sheets)) stop("В файле нет листов с профилями")
  result <- lapply(sheets, function(sheet) {
    normalise_profile_sheet(openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE), sheet)
  })
  names(result) <- sheets
  result
}

profile_catalog <- function(profiles) {
  do.call(rbind, unname(profiles))
}

profile_kits <- function(profile, temperature) {
  x <- profile[profile$annealing_temp == temperature, , drop = FALSE]
  normalise_kits(x[names(default_kits())])
}

split_kits_by_lanes <- function(kits, orientation = c("horizontal", "vertical"), avoid_edges = TRUE) {
  orientation <- match.arg(orientation)
  lane_count <- if (orientation == "horizontal") {
    if (avoid_edges) 6L else 8L
  } else {
    if (avoid_edges) 10L else 12L
  }
  split(kits, ceiling(seq_len(nrow(kits)) / lane_count))
}

partition_kits_for_full_samples <- function(samples, kits, avoid_edges = TRUE,
                                            sample_repeats = 1L, pko_repeats = 1L,
                                            oko_repeats = 1L, orientation = "horizontal",
                                            grouping = "by_kit", balance = "mirrored",
                                            controls_position = "end") {
  groups <- list()
  current <- kits[0, , drop = FALSE]
  fits_all <- function(candidate) {
    plate <- tryCatch(
      build_plate(samples, candidate, avoid_edges, sample_repeats, pko_repeats,
                  oko_repeats, orientation, grouping, balance, controls_position),
      error = function(e) NULL
    )
    !is.null(plate) && !length(attr(plate, "plan")$omitted)
  }

  for (i in seq_len(nrow(kits))) {
    candidate <- rbind(current, kits[i, , drop = FALSE])
    if (nrow(current) == 0L || fits_all(candidate)) {
      current <- candidate
    } else {
      groups[[length(groups) + 1L]] <- current
      current <- kits[i, , drop = FALSE]
    }
  }
  if (nrow(current)) groups[[length(groups) + 1L]] <- current
  groups
}

build_plate_series <- function(samples, kits, avoid_edges = TRUE, sample_repeats = 1L,
                               pko_repeats = 1L, oko_repeats = 1L,
                               orientation = "horizontal", grouping = "by_kit",
                               balance = "mirrored", controls_position = "end") {
  pending <- unique(trimws(as.character(samples)))
  pending <- pending[nzchar(pending)]
  result <- list()
  while (length(pending)) {
    # `by_kit` may have a lower effective capacity than 96 wells because a
    # whole kit block cannot be split between its designated lanes.  Find the
    # largest prefix that can be laid out, then continue on the next plate.
    plate <- NULL
    for (n in rev(seq_along(pending))) {
      candidate <- tryCatch(
        build_plate(head(pending, n), kits, avoid_edges, sample_repeats, pko_repeats,
                    oko_repeats, orientation, grouping, balance, controls_position),
        error = function(e) NULL
      )
      if (!is.null(candidate) && length(attr(candidate, "plan")$placed)) {
        plate <- candidate
        break
      }
    }
    if (is.null(plate)) {
      stop("Даже один образец с выбранными повторами и контролями не помещается на плашку")
    }
    info <- attr(plate, "plan")
    result[[length(result) + 1L]] <- plate
    pending <- setdiff(pending, info$placed)
  }
  result
}
