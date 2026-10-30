library(shiny)
library(DT)
library(openxlsx)
library(shinyMolBio)

source(file.path("R", "plate_logic.R"), local = TRUE)
source(file.path("R", "profile_logic.R"), local = TRUE)
source(file.path("R", "export_formats.R"), local = TRUE)
APP_VERSION <- "2026.10.30-18"

dir.create("data", showWarnings = FALSE)
profiles_file <- file.path("data", "kits.xlsx")
if (!file.exists(profiles_file)) {
  openxlsx::write.xlsx(list("Профиль 1" = default_kits()), profiles_file, overwrite = TRUE)
}

ui <- navbarPage(
  title = paste("ПЦР-плашка 96", APP_VERSION),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2563eb"),
  header = tags$head(
    tags$style(HTML(".container-fluid { max-width: 1600px; } .card { margin-bottom: 1rem; }
      .plate-toolbar .btn { margin-right: .35rem; margin-bottom: .35rem; }
      .status-box { padding: .8rem 1rem; border-radius: .5rem; background: #f1f5f9; margin-bottom: 1rem; }
      td.pcr-well { min-width: 72px; height: 58px; font-size: 11px; }
      .manual-marker { position:absolute; top:2px; left:3px; color:#111827; font-size:12px; line-height:12px; }")),
    tags$script(HTML("$(document).on('change.explicitPlateSelection', '#plate.pcr-plate', function() {
        var ids = $(this).find('td.selected-well').map(function() { return this.id; }).get();
        Shiny.setInputValue('plate_explicit_selection', ids.length ? ids : [''], {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('clearPlateSelection', function(id) {
        $('#' + id + '.pcr-plate').find('td.selected-well').removeClass('selected-well');
        Shiny.setInputValue(id + '_explicit_selection', [''], {priority: 'event'});
      });"))
  ),
  tabPanel("Образцы и профили",
    fluidPage(br(), fluidRow(
      column(3, bslib::card(bslib::card_header("Образцы"),
        textAreaInput("samples_source", "По одному в строке", rows = 16,
                      placeholder = "S-001\nS-002\nS-003"),
        actionButton("load_samples", "Загрузить в таблицу", class = "btn-primary w-100"), br(), br(),
        selectInput("bulk_profile", "Профиль для всех образцов", choices = NULL),
        actionButton("assign_profile_all", "Назначить всем", class = "w-100"), br(), br(),
        actionButton("clear_profile_all", "Снять у всех", class = "btn-outline-secondary w-100"), br(), br(),
        tags$small("Отметьте профиль в строке образца. Один образец можно назначить нескольким профилям."))),
      column(9, bslib::card(bslib::card_header("Назначение образцов профилям"), DTOutput("assignments_table")),
        bslib::card(bslib::card_header("Размещение реакций"), DTOutput("distribution_table")))
    ))
  ),
  tabPanel("Плашки",
    fluidPage(br(), fluidRow(
      column(3, bslib::card(bslib::card_header("Параметры расчёта"),
        fluidRow(
          column(4, numericInput("sample_repeats", "Повт. образца", 1, min = 1, max = 12)),
          column(4, numericInput("pko_repeats", "Повт. каждого ПКО", 1, min = 0, max = 12)),
          column(4, numericInput("oko_repeats", "Повт. ОКО", 1, min = 0, max = 12))
        ),
        radioButtons("orientation", "Направление", inline = TRUE,
                     choices = c("Горизонтально" = "horizontal", "Вертикально" = "vertical")),
        radioButtons("grouping", "Размещение", inline = TRUE,
                     choices = c("По наборам" = "by_kit", "Сплошное" = "continuous")),
        radioButtons("balance", "Несколько рядов/колонок", inline = TRUE,
                     choices = c("Зеркально" = "mirrored", "Рядом" = "adjacent")),
        radioButtons("controls_position", "ПКО и ОКО", inline = TRUE,
                     choices = c("В конце блока набора" = "end", "В начале блока набора" = "beginning"), selected = "end"),
        checkboxInput("avoid_edges", "Сначала заполнять внутренние лунки", TRUE),
        actionButton("calculate", "Создать плашки", class = "btn-primary w-100"), hr(),
        selectInput("export_format", "Формат экспорта", choices = c(
          "Рабочая книга XLSX" = "internal_xlsx", "Innova Sample Set XLSX" = "innova_xlsx",
          "Bio-Rad Spreadsheet CSV" = "biorad_csv"), selected = "innova_xlsx"),
        uiOutput("downloads"), hr(), uiOutput("batch_summary")
      )),
      column(9,
        selectInput("active_plate", "Плашка", choices = character()),
        div(class = "plate-toolbar", actionButton("edit_well", "Изменить выбранную"),
          actionButton("clear_wells", "Очистить выбранные"),
          actionButton("extract_wells", "Изъять выбранное", class = "btn-warning"),
          actionButton("assign_area", "Назначить область набору", class = "btn-primary"),
          actionButton("add_plate", "Добавить плашку"),
          actionButton("swap_wells", "Поменять 2 лунки местами"), actionButton("undo", "Отменить правку")),
        uiOutput("plate_ui"), hr(), DTOutput("plate_table")
      )
    ))
  ),
  tabPanel("Профили",
    fluidPage(br(), p("Каждый лист файла kits.xlsx — отдельный профиль; строка листа — набор."),
      DTOutput("profiles_table"), br(),
      downloadButton("download_profiles", "Скачать kits.xlsx"),
      fileInput("upload_profiles", "Заменить kits.xlsx", accept = ".xlsx"))
  ),
  tabPanel("Справка", fluidPage(br(),
    p("1. На вкладке «Образцы и профили» загрузите образцы и отметьте нужные профили."),
    p("2. На вкладке «Плашки» создайте раскладку. Для каждого профиля и каждой температуры отжига создаётся отдельная серия плашек."),
    p("Если реакций больше вместимости, автоматически создаются следующие плашки. Каждый файл скачивается отдельно.")))
)

server <- function(input, output, session) {
  profiles <- reactiveVal(read_profiles(profiles_file))
  samples <- reactiveVal(character())
  assignments <- reactiveVal(list())
  plates <- reactiveVal(list())
  history <- reactiveVal(list())

  observeEvent(profiles(), {
    updateSelectInput(session, "bulk_profile", choices = names(profiles()))
  }, ignoreInit = FALSE)

  assignment_matrix <- reactive({
    s <- samples(); p <- names(profiles()); a <- assignments()
    result <- data.frame("Образец" = s, check.names = FALSE, stringsAsFactors = FALSE)
    for (profile in p) {
      result[[profile]] <- vapply(s, function(sample) {
        checked <- profile %in% (a[[sample]] %||% character())
        sprintf("<input class='profile-check' type='checkbox' data-sample='%s' data-profile='%s'%s>",
                htmltools::htmlEscape(sample, attribute = TRUE), htmltools::htmlEscape(profile, attribute = TRUE),
                if (checked) " checked" else "")
      }, character(1))
    }
    result
  })

  observeEvent(input$load_samples, {
    values <- parse_samples(input$samples_source)
    if (!length(values)) return(showNotification("Введите хотя бы один образец.", type = "warning"))
    old <- assignments(); assignments(old[intersect(names(old), values)])
    samples(values)
  })

  observeEvent(input$profile_assignment, {
    change <- input$profile_assignment
    if (is.null(change$sample) || is.null(change$profile)) return()
    a <- assignments(); current <- a[[change$sample]] %||% character()
    a[[change$sample]] <- if (isTRUE(change$checked)) unique(c(current, change$profile)) else setdiff(current, change$profile)
    assignments(a)
  })

  observeEvent(input$assign_profile_all, {
    profile <- input$bulk_profile
    if (!nzchar(profile %||% "")) return()
    a <- assignments()
    for (sample in samples()) a[[sample]] <- unique(c(a[[sample]] %||% character(), profile))
    assignments(a)
  })

  observeEvent(input$clear_profile_all, {
    profile <- input$bulk_profile
    if (!nzchar(profile %||% "")) return()
    a <- assignments()
    for (sample in samples()) a[[sample]] <- setdiff(a[[sample]] %||% character(), profile)
    assignments(a)
  })

  output$assignments_table <- renderDT({
    datatable(assignment_matrix(), escape = FALSE, rownames = FALSE, selection = "none",
      callback = JS("table.on('change', 'input.profile-check', function() {
        Shiny.setInputValue('profile_assignment', {sample: $(this).data('sample'), profile: $(this).data('profile'), checked: this.checked}, {priority: 'event'});
      });"), options = list(pageLength = 25, scrollX = TRUE, dom = 'tip'))
  })

  profile_samples <- function(profile) {
    a <- assignments()
    samples()[vapply(samples(), function(sample) profile %in% (a[[sample]] %||% character()), logical(1))]
  }

  placed_reactions <- function(profile, kit) {
    x <- plates()
    rows <- lapply(x, function(record) {
      if (!identical(record$profile, profile)) return(NULL)
      plate <- record$plate
      plate[plate$kit == kit & plate$type != "Empty", , drop = FALSE]
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(empty_plate()[0, ])
    do.call(rbind, rows)
  }

  output$distribution_table <- renderDT({
    ps <- profiles(); rows <- list()
    for (profile in names(ps)) {
      assigned <- profile_samples(profile)
      for (kit in ps[[profile]]$kit) {
        placed <- placed_reactions(profile, kit)
        for (sample in assigned) {
          done <- sum(placed$type == "Sample" & placed$sample == sample)
          target <- input$sample_repeats %||% 1L
          rows[[length(rows) + 1L]] <- data.frame(
            "Образец" = sample, "Профиль" = profile, "Набор" = kit,
            "Размещено" = done, "Осталось" = max(0L, target - done),
            "Статус" = if (done >= target) "Готово" else "Не распределён",
            check.names = FALSE
          )
        }
      }
    }
    x <- if (length(rows)) do.call(rbind, rows) else data.frame(
      "Образец"=character(), "Профиль"=character(), "Набор"=character(),
      "Размещено"=integer(), "Осталось"=integer(), "Статус"=character(), check.names = FALSE)
    datatable(x, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$profiles_table <- renderDT({
    x <- profile_catalog(profiles())
    datatable(x[c("profile", "kit", "annealing_temp", "channels")], rownames = FALSE,
      colnames = c("Профиль", "Набор", "Ta", "Каналы"), options = list(pageLength = 25))
  })

  observeEvent(input$upload_profiles, {
    req(input$upload_profiles$datapath)
    tryCatch({
      candidate <- read_profiles(input$upload_profiles$datapath)
      file.copy(input$upload_profiles$datapath, profiles_file, overwrite = TRUE)
      profiles(candidate)
      a <- assignments(); valid <- names(candidate)
      assignments(lapply(a, intersect, y = valid))
      showNotification("Профили загружены.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  })
  output$download_profiles <- downloadHandler("kits.xlsx", function(file) file.copy(profiles_file, file, overwrite = TRUE))

  save_history <- function() history(append(history(), list(plates())))
  current_record <- reactive({
    x <- plates(); req(length(x), nzchar(input$active_plate %||% "")); x[[input$active_plate]]
  })
  current_plate <- reactive(current_record()$plate)
  update_current_plate <- function(value) {
    x <- plates(); x[[input$active_plate]]$plate <- value; plates(x)
  }
  update_current_record <- function(value) {
    x <- plates(); x[[input$active_plate]] <- value; plates(x)
  }
  reaction_key <- function(plate) {
    paste(plate$type, plate$sample, plate$kit, plate$replicate, sep = "\r")
  }
  clear_duplicate_reactions <- function(plate, locked_keys) {
    used <- which(plate$type != "Empty" & reaction_key(plate) %in% locked_keys)
    if (!length(used)) return(plate)
    plate[used, c("sample", "kit", "channels")] <- ""
    plate$type[used] <- "Empty"; plate$replicate[used] <- NA_integer_
    plate$layout_order[used] <- NA_integer_; plate$manual[used] <- FALSE
    plate
  }
  refresh_plate_choices <- function(selected = NULL) {
    x <- plates()
    if (!length(x)) return(updateSelectInput(session, "active_plate", choices = character()))
    labels <- vapply(x, function(record) {
      if (!is.null(record$title)) return(record$title)
      sprintf("%s — %s — плашка %d/%d", record$profile, record$temperature, record$number, record$total)
    }, character(1))
    updateSelectInput(session, "active_plate", choices = stats::setNames(names(x), labels),
                      selected = selected %||% names(x)[1])
  }

  observeEvent(input$calculate, {
    req(length(samples()))
    a <- assignments(); ps <- profiles(); result <- list()
    tryCatch({
      for (profile_name in names(ps)) {
        profile_samples <- samples()[vapply(samples(), function(s) profile_name %in% (a[[s]] %||% character()), logical(1))]
        if (!length(profile_samples)) next
        profile <- ps[[profile_name]]
        for (temperature in unique(profile$annealing_temp)) {
          kits <- profile_kits(profile, temperature)
          kit_groups <- if (identical(input$grouping, "by_kit")) {
            partition_kits_for_full_samples(profile_samples, kits, input$avoid_edges,
                                            input$sample_repeats, input$pko_repeats,
                                            input$oko_repeats, input$orientation,
                                            input$grouping, input$balance, input$controls_position)
          } else list(kits)
          for (group_number in seq_along(kit_groups)) {
            group_kits <- kit_groups[[group_number]]
            series <- build_plate_series(profile_samples, group_kits, input$avoid_edges, input$sample_repeats,
                                         input$pko_repeats, input$oko_repeats, input$orientation,
                                         input$grouping, input$balance, input$controls_position)
            for (i in seq_along(series)) {
              id <- paste0("plate_", length(result) + 1L)
              result[[id]] <- list(plate = series[[i]], profile = profile_name, temperature = temperature,
                                   number = i, total = length(series), kits = group_kits,
                                   kit_group = group_number, kit_groups_total = length(kit_groups))
            }
          }
        }
      }
      if (!length(result)) stop("Не отмечен ни один профиль для образцов")
      # Keep manually assigned plates intact. The same reaction is removed
      # from the new automatic plan, so a manual allocation is never lost or
      # duplicated after recalculation.
      old <- plates()
      locked <- Filter(function(record) isTRUE(record$manual), old)
      locked_reactions <- lapply(locked, function(record) record$plate[record$plate$type != "Empty", , drop = FALSE])
      locked_reactions <- Filter(function(x) nrow(x) > 0L, locked_reactions)
      locked_keys <- if (length(locked_reactions)) unique(unlist(lapply(locked_reactions, reaction_key), use.names = FALSE)) else character()
      if (length(locked_keys)) {
        result <- lapply(result, function(record) {
          record$plate <- clear_duplicate_reactions(record$plate, locked_keys)
          record
        })
        result <- Filter(function(record) any(record$plate$type != "Empty"), result)
      }
      if (length(locked) && length(result)) {
        names(result) <- make.unique(c(names(locked), paste0("auto_", names(result))))[-seq_along(locked)]
      }
      result <- c(locked, result)
      if (!length(result)) stop("Все реакции уже размещены на закреплённых ручных плашках")
      plates(result); history(list())
      refresh_plate_choices(if (length(locked)) names(locked)[1] else names(result)[1])
      showNotification(paste("Создано плашек:", length(result)), type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 10))
  })

  selected_positions <- reactive({ value <- as.character(input$plate_explicit_selection %||% character()); unique(value[nzchar(value)]) })
  observeEvent(current_plate(), session$sendCustomMessage("clearPlateSelection", "plate"), ignoreInit = FALSE)

  observeEvent(input$add_plate, {
    save_history()
    x <- plates()
    id <- paste0("manual_plate_", length(x) + 1L)
    while (id %in% names(x)) id <- paste0(id, "_new")
    x[[id]] <- list(plate = empty_plate(), profile = "Ручная", temperature = "",
                    number = 1L, total = 1L, kits = default_kits()[0, ],
                    title = paste("Ручная плашка", length(x) + 1L), manual = TRUE)
    plates(x)
    refresh_plate_choices(id)
    showNotification("Добавлена пустая плашка. Выделите область и назначьте ей набор.", type = "message")
  })

  output$area_kit_ui <- renderUI({
    profile <- input$area_profile
    choices <- if (nzchar(profile %||% "") && profile %in% names(profiles())) profiles()[[profile]]$kit else character()
    selectInput("area_kit", "Набор", choices = choices)
  })

  observeEvent(input$assign_area, {
    pos <- selected_positions()
    if (!length(pos)) return(showNotification("Выделите пустую область на плашке.", type = "warning"))
    x <- current_plate()
    chosen <- x[match(pos, x$position), , drop = FALSE]
    if (any(chosen$type != "Empty")) return(showNotification("Область для назначения должна состоять только из пустых лунок.", type = "warning"))
    showModal(modalDialog(
      title = paste("Назначить область (", length(pos), " лунок)", sep = ""),
      selectInput("area_profile", "Профиль", choices = names(profiles())),
      uiOutput("area_kit_ui"),
      tags$small("Будут добавлены только ещё не размещённые образцы и контроли этого набора."),
      footer = tagList(modalButton("Отмена"), actionButton("fill_area", "Заполнить область", class = "btn-primary"))
    ))
  })

  observeEvent(input$fill_area, {
    pos <- selected_positions(); profile <- input$area_profile; kit_name <- input$area_kit
    if (!length(pos) || !nzchar(profile %||% "") || !nzchar(kit_name %||% "")) return()
    record <- current_record()
    if (!identical(record$profile, "Ручная") && !identical(record$profile, profile)) {
      return(showNotification("На одной ручной плашке можно размещать наборы только одного профиля.", type = "warning"))
    }
    profile_data <- profiles()[[profile]]
    kit <- profile_kits(profile_data, profile_data$annealing_temp[match(kit_name, profile_data$kit)])
    kit <- kit[kit$kit == kit_name, , drop = FALSE]
    x <- current_plate()
    target <- x[match(pos, x$position), , drop = FALSE]
    if (any(target$type != "Empty")) return(showNotification("Часть выбранной области уже занята. Повторите выделение.", type = "warning"))
    existing <- placed_reactions(profile, kit_name)
    assigned <- profile_samples(profile)
    sample_rows <- do.call(rbind, lapply(assigned, function(sample) {
      already <- sum(existing$type == "Sample" & existing$sample == sample)
      remaining <- max(0L, (input$sample_repeats %||% 1L) - already)
      if (!remaining) return(NULL)
      data.frame(sample = sample, kit = kit_name, type = "Sample", replicate = already + seq_len(remaining), channels = kit$channels, stringsAsFactors = FALSE)
    }))
    if (is.null(sample_rows)) sample_rows <- data.frame(sample=character(), kit=character(), type=character(), replicate=integer(), channels=character())
    full_controls <- make_reactions(character(), kit, 1L, input$pko_repeats, input$oko_repeats,
                                    controls_position = input$controls_position)$reactions
    control_rows <- do.call(rbind, lapply(split(seq_len(nrow(full_controls)), paste(full_controls$type, full_controls$sample)), function(index) {
      already <- sum(existing$type == full_controls$type[index[1]] & existing$sample == full_controls$sample[index[1]])
      head(full_controls[index, , drop = FALSE], max(0L, length(index) - already))
    }))
    if (is.null(control_rows)) control_rows <- full_controls[0, , drop = FALSE]
    reactions <- if (identical(input$controls_position, "beginning")) rbind(control_rows, sample_rows) else rbind(sample_rows, control_rows)
    if (!nrow(reactions)) return(showNotification("Для выбранного профиля и набора не осталось нераспределённых реакций.", type = "message"))
    take <- min(length(pos), nrow(reactions))
    save_history()
    indices <- match(pos[seq_len(take)], x$position)
    x[indices, c("sample", "kit", "type", "replicate", "channels")] <- reactions[seq_len(take), ]
    next_order <- max(c(0L, x$layout_order), na.rm = TRUE) + seq_len(take)
    next_slot <- max(c(0L, x$layout_slot), na.rm = TRUE) + seq_len(take)
    x$layout_order[indices] <- next_order; x$layout_slot[indices] <- next_slot; x$layout_kit[indices] <- kit_name; x$manual[indices] <- TRUE
    if (!(kit_name %in% record$kits$kit)) record$kits <- rbind(record$kits, kit)
    if (identical(record$profile, "Ручная")) record$profile <- profile
    record$manual <- TRUE
    record$plate <- x
    update_current_record(record)
    removeModal()
    message <- if (take < nrow(reactions)) paste("Заполнено", take, "лунок; осталось реакций:", nrow(reactions) - take) else paste("Заполнено лунок:", take)
    showNotification(message, type = "message")
  })

  output$plate_ui <- renderUI({
    req(length(plates())); x <- plate_for_widget(current_plate())
    shinyMolBio::pcrPlateInput("plate", "Интерактивная плашка (клик — выбор)", x,
      wellLabelTemplate = "{{{label}}}", wellStyleTemplate = "{{style}}", wellGroupTemplate = "{{sample}}-{{kit}}",
      onHoverWellTextTemplate = "{{position}}\n{{sample}}\n{{kit}}\n{{type_label}}, повтор {{replicate}}\nКаналы: {{channels}}", interactive = TRUE)
  })
  output$plate_table <- renderDT({
    req(length(plates())); x <- current_plate(); x <- x[x$type != "Empty", ]; x <- x[order(x$layout_order, na.last = TRUE), ]
    datatable(x[c("position", "sample", "kit", "type", "replicate", "channels")], rownames = FALSE, options = list(pageLength = 10),
      colnames = c("Лунка", "Образец", "Набор", "Тип", "Повтор", "Каналы"))
  })
  output$batch_summary <- renderUI({
    x <- plates(); if (!length(x)) return(tags$small("Плашки ещё не созданы."))
    tags$div(class = "status-box", tags$b(sprintf("Создано плашек: %d", length(x))), tags$br(),
      paste(vapply(x, function(p) sprintf("%s (%s): %d/96", p$profile, p$temperature, sum(p$plate$type != "Empty")), character(1)), collapse = "; "))
  })

  observeEvent(input$clear_wells, {
    pos <- selected_positions(); if (!length(pos)) return(showNotification("Сначала выберите нужные лунки кликом на плашке.", type = "warning"))
    save_history(); x <- current_plate(); i <- match(pos, x$position, nomatch = 0L); i <- i[i > 0L]
    x[i, c("sample", "kit", "channels")] <- ""; x$type[i] <- "Empty"; x$replicate[i] <- NA_integer_; x$layout_order[i] <- NA_integer_; x$manual[i] <- FALSE; update_current_plate(x)
  })
  observeEvent(input$extract_wells, {
    pos <- selected_positions(); if (!length(pos)) return(showNotification("Сначала выберите нужные лунки кликом на плашке.", type = "warning"))
    x <- current_plate(); occupied <- pos[pos %in% x$position[x$type != "Empty"]]
    if (!length(occupied)) return(showNotification("В выбранных лунках нет реакций для изъятия.", type = "warning"))
    save_history(); update_current_plate(compact_plate(x, occupied, preserve_kits = identical(input$grouping, "by_kit")))
  })
  observeEvent(input$swap_wells, {
    pos <- selected_positions(); if (length(pos) != 2L) return(showNotification("Выберите ровно две лунки.", type = "warning"))
    save_history(); x <- current_plate(); i <- match(pos, x$position); cols <- c("sample", "kit", "type", "replicate", "channels", "layout_order", "manual")
    tmp <- x[i[1], cols]; x[i[1], cols] <- x[i[2], cols]; x[i[2], cols] <- tmp; update_current_plate(x)
  })
  observeEvent(input$edit_well, {
    pos <- selected_positions(); if (length(pos) != 1L) return(showNotification("Выберите одну лунку.", type = "warning"))
    row <- current_plate()[match(pos, current_plate()$position), ]; record <- current_record()
    showModal(modalDialog(title = paste("Лунка", pos), textInput("well_sample", "Образец", row$sample),
      selectInput("well_kit", "Набор", choices = c("", record$kits$kit), selected = row$kit),
      selectInput("well_type", "Тип", c("Образец" = "Sample", "ПКО" = "PCO", "ОКО" = "NTC", "Пусто" = "Empty"), selected = row$type),
      numericInput("well_replicate", "Повтор", ifelse(is.na(row$replicate), 1, row$replicate), min = 1),
      footer = tagList(modalButton("Отмена"), actionButton("save_well", "Сохранить", class = "btn-primary"))))
  })
  observeEvent(input$save_well, {
    pos <- selected_positions(); req(length(pos) == 1L); save_history(); x <- current_plate(); i <- match(pos, x$position)
    if (input$well_type == "Empty") { x[i, c("sample", "kit", "channels")] <- ""; x$type[i] <- "Empty"; x$replicate[i] <- NA_integer_; x$layout_order[i] <- NA_integer_; x$manual[i] <- FALSE }
    else { x$sample[i] <- trimws(input$well_sample); x$kit[i] <- input$well_kit; x$type[i] <- input$well_type; x$replicate[i] <- as.integer(input$well_replicate); x$channels[i] <- current_record()$kits$channels[match(input$well_kit, current_record()$kits$kit)]; if (is.na(x$layout_order[i])) x$layout_order[i] <- max(c(0L, x$layout_order), na.rm = TRUE) + 1L }
    update_current_plate(x); removeModal()
  })
  observeEvent(input$undo, { h <- history(); if (!length(h)) return(showNotification("Нет изменений для отмены.", type = "message")); plates(h[[length(h)]]); history(h[-length(h)]) })

  output$downloads <- renderUI({
    x <- plates(); if (!length(x)) return(NULL)
    tagList(lapply(names(x), function(id) downloadButton(paste0("download_", id), paste("Скачать", id), class = "w-100 mb-1")))
  })
  observeEvent(plates(), {
    x <- plates()
    for (id in names(x)) local({
      plate_id <- id
      output[[paste0("download_", plate_id)]] <- downloadHandler(
        filename = function() {
          p <- plates()[[plate_id]]
          base <- paste0(gsub("[^[:alnum:]]+", "_", p$profile), "_", gsub("[^[:alnum:]]+", "_", p$temperature), "_", p$number)
          switch(input$export_format, innova_xlsx = paste0("Innova_", base, ".xlsx"), biorad_csv = paste0("BioRad_", base, ".csv"), paste0("PCR_", base, ".xlsx"))
        },
        content = function(file) {
          p <- plates()[[plate_id]]
          switch(input$export_format, innova_xlsx = write_innova_xlsx(p$plate, file), biorad_csv = write_biorad_csv(p$plate, file), write_internal_xlsx(p$plate, p$kits, file))
        })
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
