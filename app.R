library(shiny)
library(DT)
library(openxlsx)
library(shinyMolBio)

source(file.path("R", "plate_logic.R"), local = TRUE)
source(file.path("R", "export_formats.R"), local = TRUE)
APP_VERSION <- "2026.10.27-11"

dir.create("data", showWarnings = FALSE)
kits_file <- file.path("data", "kits.xlsx")
if (!file.exists(kits_file)) openxlsx::write.xlsx(default_kits(), kits_file, overwrite = TRUE)

read_kits <- function(path = kits_file) normalise_kits(openxlsx::read.xlsx(path))
write_kits <- function(x) openxlsx::write.xlsx(normalise_kits(x), kits_file, overwrite = TRUE)

# Persist the migrated schema immediately so the workbook always mirrors the UI.
write_kits(read_kits())

ui <- navbarPage(
  title = paste("ПЦР-плашка 96", APP_VERSION),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2563eb"),
  header = tags$head(tags$style(HTML("
    .container-fluid { max-width: 1600px; }
    .card { margin-bottom: 1rem; }
    .plate-toolbar .btn { margin-right: .35rem; margin-bottom: .35rem; }
    .status-box { padding: .8rem 1rem; border-radius: .5rem; background: #f1f5f9; margin-bottom: 1rem; }
    td.pcr-well { min-width: 72px; height: 58px; font-size: 11px; }
  "))),
  tabPanel("Планирование",
    fluidPage(br(), fluidRow(
      column(3,
        bslib::card(
          bslib::card_header("Исходные данные"),
          selectizeInput("selected_kits", "Наборы ПЦР-РВ", choices = NULL, multiple = TRUE),
          textAreaInput("samples", "Образцы (по одному в строке)", rows = 12,
                        placeholder = "S-001\nS-002\nS-003"),
          fluidRow(
            column(4, numericInput("sample_repeats", "Повт. образца", 2, min=1, max=12)),
            column(4, numericInput("pko_repeats", "Повт. каждого ПКО", 1, min=0, max=12)),
            column(4, numericInput("oko_repeats", "Повт. ОКО", 1, min=0, max=12))
          ),
          radioButtons("orientation", "Направление", inline = TRUE,
                       choices = c("Горизонтально"="horizontal", "Вертикально"="vertical"),
                       selected = "horizontal"),
          radioButtons("grouping", "Размещение", inline = TRUE,
                       choices = c("По наборам"="by_kit", "Сплошное"="continuous"),
                       selected = "by_kit"),
          radioButtons("balance", "Несколько рядов/колонок", inline = TRUE,
                       choices = c("Зеркально"="mirrored", "Рядом"="adjacent"),
                       selected = "mirrored"),
          checkboxInput("avoid_edges", "Сначала заполнять внутренние лунки", TRUE),
          actionButton("calculate", "Рассчитать плашку", class = "btn-primary w-100"),
          hr(),
          selectInput("export_format", "Формат экспорта",
                      choices=c("Рабочая книга XLSX"="internal_xlsx",
                                "Innova Sample Set XLSX"="innova_xlsx",
                                "Bio-Rad Spreadsheet CSV"="biorad_csv")),
          downloadButton("download_plate", "Скачать для прибора", class = "w-100")
        ),
        uiOutput("summary")
      ),
      column(9,
        div(class = "plate-toolbar",
          actionButton("edit_well", "Изменить выбранную"),
          actionButton("clear_wells", "Очистить выбранные"),
          actionButton("extract_wells", "Изъять выбранное", class = "btn-warning"),
          actionButton("swap_wells", "Поменять 2 лунки местами"),
          actionButton("undo", "Отменить правку")
        ),
        uiOutput("plate_ui"),
        hr(),
        DTOutput("plate_table")
      )
    ))
  ),
  tabPanel("Наборы",
    fluidPage(br(),
      p("Одна строка — один набор. ПКО — положительный контроль, ОКО — отрицательный контроль."),
      DTOutput("kits_table"), br(),
      actionButton("add_kit", "Добавить набор", class = "btn-primary"),
      actionButton("edit_kit", "Изменить выбранный"),
      actionButton("delete_kit", "Удалить выбранный", class = "btn-danger"),
      downloadButton("download_kits", "Скачать Excel"),
      fileInput("upload_kits", "Заменить из Excel", accept = ".xlsx")
    )
  ),
  tabPanel("Справка", fluidPage(br(),
    h3("Как работать"),
    tags$ol(
      tags$li("Настройте наборы на вкладке «Наборы»."),
      tags$li("Выберите наборы, вставьте список образцов и рассчитайте раскладку."),
      tags$li("Выберите одну или две лунки кликом на плашке для ручной правки."),
      tags$li("Выберите формат экспорта: рабочий XLSX, Innova XLSX или Bio-Rad CSV для Spreadsheet View/Importer.")
    ),
    p("Bio-Rad PLTD является зашифрованным форматом. CSV загружается в CFX Manager через Plate Editor → Spreadsheet View/Importer → Import.")
  ))
)

server <- function(input, output, session) {
  kits <- reactiveVal(read_kits())
  plate <- reactiveVal(empty_plate())
  history <- reactiveVal(list())
  plan_info <- reactiveVal(list(placed = character(), omitted = character(), controls = 0L))
  layout_grouping <- reactiveVal("by_kit")

  observeEvent(kits(), {
    x <- kits()
    current <- isolate(input$selected_kits) %||% character()
    updateSelectizeInput(session, "selected_kits", choices = x$kit,
                         selected = intersect(current, x$kit), server = TRUE)
  }, ignoreInit = FALSE)

  save_history <- function() history(append(history(), list(plate())))

  observeEvent(input$calculate, {
    req(length(input$selected_kits))
    sample_ids <- parse_samples(input$samples)
    req(length(sample_ids))
    selected <- kits()[kits()$kit %in% input$selected_kits, , drop = FALSE]
    tryCatch({
      if (is.null(input$sample_repeats) || is.na(input$sample_repeats) || input$sample_repeats < 1) {
        stop("Укажите число повторов образца (не меньше 1)")
      }
      result <- build_plate(sample_ids, selected, input$avoid_edges,
                            input$sample_repeats, input$pko_repeats, input$oko_repeats,
                            input$orientation, input$grouping, input$balance)
      info <- attr(result, "plan")
      if (length(info$omitted)) {
        stop("Все образцы не помещаются на одну 96-луночную плашку: ",
             paste(info$omitted, collapse = ", "),
             ". Уменьшите число повторов/наборов или разделите запуск.")
      }
      history(list())
      plan_info(info)
      layout_grouping(input$grouping)
      plate(result)
      if (length(info$omitted)) showNotification(
        paste(length(info$omitted), "образцов не поместились; см. сводку."), type = "warning", duration = 8)
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  })

  output$summary <- renderUI({
    x <- plate(); info <- plan_info(); occupied <- sum(x$type != "Empty")
    div(class = "status-box",
      tags$b(sprintf("Занято: %d / 96", occupied)), tags$br(),
      sprintf("Реакций образцов: %d", sum(x$type == "Sample")), tags$br(),
      sprintf("Образцов размещено: %d", length(info$placed %||% character())), tags$br(),
      if (length(info$placed %||% character()))
        tags$small(paste("На плашке:", paste(info$placed, collapse = ", "))),
      tags$br(), tags$small(paste("Алгоритм", APP_VERSION)),
      if (length(info$omitted %||% character())) tags$span(style="color:#b91c1c",
        paste("Не поместились:", paste(info$omitted, collapse = ", ")))
    )
  })

  output$plate_ui <- renderUI({
    x <- plate_for_widget(plate())
    shinyMolBio::pcrPlateInput(
      "plate", "Интерактивная плашка (клик — выбор)", x,
      wellLabelTemplate = "{{{label}}}",
      onHoverWellTextTemplate = "{{position}}\n{{sample}}\n{{kit}}\n{{type_label}}, повтор {{replicate}}\nКаналы: {{channels}}",
      wellStyleTemplate = "{{style}}", wellGroupTemplate = "{{sample}}-{{kit}}",
      interactive = TRUE
    )
  })

  selected_positions <- reactive({
    value <- input$plate
    if (is.null(value)) character() else as.character(value)
  })

  observeEvent(input$clear_wells, {
    pos <- selected_positions(); req(length(pos))
    if (length(pos) > 12L) return(showNotification("Сначала выберите нужные лунки кликом на плашке.", type = "warning"))
    save_history(); x <- plate(); idx <- match(pos, x$position, nomatch = 0L); idx <- idx[idx > 0L]
    x[idx, c("sample", "kit", "channels")] <- ""
    x$type[idx] <- "Empty"; x$replicate[idx] <- NA_integer_; x$layout_order[idx] <- NA_integer_; plate(x)
  })

  observeEvent(input$extract_wells, {
    pos <- selected_positions(); req(length(pos))
    if (length(pos) > 12L) return(showNotification("Сначала выберите нужные лунки кликом на плашке.", type = "warning"))
    x <- plate()
    occupied <- pos[pos %in% x$position[x$type != "Empty"]]
    if (!length(occupied)) return(showNotification("В выбранных лунках нет реакций для изъятия.", type = "warning"))
    save_history()
    x <- compact_plate(x, occupied, preserve_kits = identical(layout_grouping(), "by_kit"))
    plate(x)
    info <- plan_info()
    actual_samples <- unique(x$sample[x$type == "Sample"])
    info$placed <- (info$placed %||% character())[info$placed %in% actual_samples]
    plan_info(info)
    showNotification(paste("Изъято реакций:", length(occupied), "— последующие лунки сдвинуты."), type = "message")
  })

  observeEvent(input$swap_wells, {
    pos <- selected_positions()
    if (length(pos) != 2L) return(showNotification("Выберите ровно две лунки.", type = "warning"))
    save_history(); x <- plate(); idx <- match(pos, x$position)
    cols <- c("sample", "kit", "type", "replicate", "channels", "layout_order")
    tmp <- x[idx[1], cols]; x[idx[1], cols] <- x[idx[2], cols]; x[idx[2], cols] <- tmp
    plate(x)
  })

  observeEvent(input$edit_well, {
    pos <- selected_positions()
    if (length(pos) != 1L) return(showNotification("Выберите одну лунку.", type = "warning"))
    row <- plate()[match(pos, plate()$position), ]
    showModal(modalDialog(title = paste("Лунка", pos),
      textInput("well_sample", "Образец", row$sample),
      selectInput("well_kit", "Набор", choices = c("", kits()$kit), selected = row$kit),
      selectInput("well_type", "Тип", c("Образец"="Sample", "ПКО"="PCO", "ОКО"="NTC", "Пусто"="Empty"), selected = row$type),
      numericInput("well_replicate", "Повтор", value = ifelse(is.na(row$replicate), 1, row$replicate), min = 1, step = 1),
      footer = tagList(modalButton("Отмена"), actionButton("save_well", "Сохранить", class="btn-primary"))))
  })

  observeEvent(input$save_well, {
    pos <- selected_positions(); req(length(pos) == 1L)
    save_history(); x <- plate(); i <- match(pos, x$position)
    if (input$well_type == "Empty") {
      x[i, c("sample", "kit", "channels")] <- ""; x$type[i] <- "Empty"; x$replicate[i] <- NA_integer_; x$layout_order[i] <- NA_integer_
    } else {
      x$sample[i] <- trimws(input$well_sample); x$kit[i] <- input$well_kit; x$type[i] <- input$well_type
      x$replicate[i] <- as.integer(input$well_replicate)
      x$channels[i] <- kits()$channels[match(input$well_kit, kits()$kit)] %||% ""
      if (is.na(x$layout_order[i])) x$layout_order[i] <- max(c(0L, x$layout_order), na.rm=TRUE) + 1L
      if (is.na(x$layout_slot[i])) x$layout_slot[i] <- max(c(0L, x$layout_slot), na.rm=TRUE) + 1L
      if (!nzchar(x$layout_kit[i])) x$layout_kit[i] <- input$well_kit
    }
    plate(x); removeModal()
  })

  observeEvent(input$undo, {
    h <- history()
    if (!length(h)) return(showNotification("Нет изменений для отмены.", type = "message"))
    plate(h[[length(h)]]); history(h[-length(h)])
  })

  output$plate_table <- renderDT({
    x <- plate(); x <- x[x$type != "Empty", ]; x <- x[order(x$layout_order, na.last=TRUE), ]
    x <- x[c("position", "sample", "kit", "type", "replicate", "channels")]
    datatable(x, rownames = FALSE, options = list(pageLength = 10),
              colnames = c("Лунка", "Образец", "Набор", "Тип", "Повтор", "Каналы"))
  })

  output$kits_table <- renderDT(datatable(kits(), selection = "single", rownames = FALSE,
    options = list(pageLength = 12),
    colnames = c("Набор", "Каналы", "ПКО_wt", "ПКО_h", "ПКО_m", "ОКО")))

  kit_dialog <- function(row = NULL) {
    if (is.null(row)) row <- default_kits()[1, ]
    showModal(modalDialog(title = "Параметры набора",
      textInput("kit_name", "Название", row$kit), textInput("kit_channels", "Каналы", row$channels),
      textInput("kit_pko_wt_name", "Название ПКО_wt", row$pko_wt_name),
      textInput("kit_pko_h_name", "Название ПКО_h", row$pko_h_name),
      textInput("kit_pko_m_name", "Название ПКО_m", row$pko_m_name),
      textInput("kit_oko_name", "Название ОКО", row$oko_name),
      footer=tagList(modalButton("Отмена"), actionButton("save_kit", "Сохранить", class="btn-primary"))))
  }
  editing_kit <- reactiveVal(NA_integer_)
  observeEvent(input$add_kit, { editing_kit(NA_integer_); kit_dialog(default_kits()[1, ]) })
  observeEvent(input$edit_kit, {
    i <- input$kits_table_rows_selected
    if (!length(i)) return(showNotification("Выберите набор в таблице.", type="warning"))
    editing_kit(i); kit_dialog(kits()[i, ])
  })
  observeEvent(input$save_kit, {
    row <- data.frame(kit=trimws(input$kit_name), channels=trimws(input$kit_channels),
      pko_wt_name=trimws(input$kit_pko_wt_name), pko_h_name=trimws(input$kit_pko_h_name),
      pko_m_name=trimws(input$kit_pko_m_name), oko_name=trimws(input$kit_oko_name), stringsAsFactors=FALSE)
    x <- kits(); i <- editing_kit(); if (is.na(i)) x <- rbind(x, row) else x[i, ] <- row
    tryCatch({ x <- normalise_kits(x); write_kits(x); kits(x); removeModal() },
             error=function(e) showNotification(conditionMessage(e), type="error"))
  })
  observeEvent(input$delete_kit, {
    i <- input$kits_table_rows_selected
    if (!length(i)) return(showNotification("Выберите набор в таблице.", type="warning"))
    x <- kits()[-i, , drop=FALSE]; write_kits(x); kits(x)
  })
  observeEvent(input$upload_kits, {
    req(input$upload_kits$datapath)
    tryCatch({ x <- read_kits(input$upload_kits$datapath); write_kits(x); kits(x); showNotification("Справочник загружен.", type="message") },
             error=function(e) showNotification(conditionMessage(e), type="error", duration=8))
  })

  output$download_kits <- downloadHandler("kits.xlsx", function(file) openxlsx::write.xlsx(kits(), file, overwrite=TRUE))
  output$download_plate <- downloadHandler(
    filename = function() switch(input$export_format,
      innova_xlsx = paste0("Innova_plate_", Sys.Date(), ".xlsx"),
      biorad_csv = paste0("BioRad_plate_", Sys.Date(), ".csv"),
      paste0("PCR_plate_", Sys.Date(), ".xlsx")),
    content = function(file) {
      switch(input$export_format,
        innova_xlsx = write_innova_xlsx(plate(), file),
        biorad_csv = write_biorad_csv(plate(), file),
        write_internal_xlsx(plate(), kits(), file))
    })
}

shinyApp(ui, server)
