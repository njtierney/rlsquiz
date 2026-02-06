# Shiny app used by coralquiz::practice() and coralquiz::quiz()
library(shiny)
library(bslib)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(here)

# -------- helpers --------
valid_ext <- c("jpg", "jpeg", "png", "webp", "JPG")
nice_species <- function(x) gsub("_", " ", x, fixed = TRUE)

collect_sources <- function(base_dir) {
  if (!dir.exists(base_dir)) {
    return(character(0))
  }
  list.dirs(base_dir, full.names = FALSE, recursive = FALSE) |> sort()
}

collect_items <- function(base_dir, source) {
  src_dir <- file.path(base_dir, source)
  if (!dir.exists(src_dir)) {
    return(tibble())
  }
  species_dirs <- list.dirs(src_dir, full.names = TRUE, recursive = FALSE)
  tibble(
    species_dir = species_dirs,
    species_key = basename(species_dirs),
    species_lab = nice_species(basename(species_dirs))
  ) |>
    mutate(images = map(species_dir, ~ list.files(.x, full.names = TRUE))) |>
    unnest(images) |>
    filter(tolower(tools::file_ext(images)) %in% valid_ext) |>
    mutate(filename = basename(images))
}

img_src <- function(base_path_name, source, species_key, filename) {
  file.path(base_path_name, source, species_key, filename)
}

# -------- runtime options from practice()/quiz() --------
opt_mode <- getOption("rlsquiz.mode", "practice") # "practice" or "quiz"
opt_n <- getOption("rlsquiz.n", NA_integer_) # number of Qs in quiz
opt_save_csv <- isTRUE(getOption("rlsquiz.save_csv", FALSE))
opt_csv_path <- getOption("rlsquiz.csv_path", getwd())
opt_user <- getOption("rlsquiz.user", NA_character_)
opt_default <- getOption("rlsquiz.default_source", NULL)

# -------- UI --------
ui <- page_sidebar(
  theme = bs_theme(
    bg = "#004d4d",
    fg = "#f4f8ff",
    primary = "#008080"
  ),

  sidebar = sidebar(
    # title = "Photo Source",
    bg = "#e8f4f8", # Light blue background
    fg = "#0b2a56", # Dark blue text
    uiOutput("source_selector_ui"),
    conditionalPanel(
      condition = sprintf("'%s' === 'quiz'", opt_mode),
      h5("Quiz Progress"),
      p(strong("Question: "), textOutput("qnum", inline = TRUE)),
      p(strong("Score: "), textOutput("score_text", inline = TRUE))
    )
  ),

  # Custom CSS
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # Main content
  h2("RLS Habitat Identification Quiz", class = "app-title"),
  div(
    class = "wrap",
    uiOutput("status_ui"),
    uiOutput("image_ui"),
    uiOutput("options_ui"),
    div(
      class = "controls",
      actionButton("next_btn", "Next", class = "btn-next")
    )
  )
)

# -------- server --------
server <- function(input, output, session) {
  # Determine photos root (bundled or external)
  external_root <- getOption("rlsquiz.photos_root", NULL)
  if (is.null(external_root)) {
    base_dir <- here::here("inst", "app", "www", "photos")
    base_path_name <- "photos"
  } else {
    base_dir <- normalizePath(external_root, mustWork = FALSE)
    base_path_name <- "userphotos"
    shiny::addResourcePath(base_path_name, base_dir)
  }

  message("[rlsquiz] Using base_dir: ", base_dir)
  message("[rlsquiz] Directory exists: ", dir.exists(base_dir))

  # Detect available sources
  available_sources <- reactive({
    if (!dir.exists(base_dir)) {
      message("[rlsquiz] base_dir does not exist!")
      return(character(0))
    }
    srcs <- collect_sources(base_dir)
    message("[rlsquiz] Sources found: ", paste(srcs, collapse = ", "))
    srcs
  })

  # Render source selector UI
  output$source_selector_ui <- renderUI({
    srcs <- available_sources()
    if (length(srcs) == 0) {
      return(NULL)
    }

    # Determine initial selection
    initial <- if (!is.null(opt_default) && opt_default %in% srcs) {
      opt_default
    } else {
      srcs[1]
    }

    if (length(srcs) == 1) {
      # Just show the name if only one option
      p(strong("Source: "), srcs[1])
    } else {
      # Show dropdown for multiple sources
      selectInput(
        "source_select",
        label = "Photo Source:",
        choices = srcs,
        selected = initial
      )
    }
  })

  # Choose source function
  choose_source <- function() {
    srcs <- available_sources()
    if (length(srcs) == 0) {
      return(NULL)
    }

    if (!is.null(opt_default) && opt_default %in% srcs) {
      return(opt_default)
    }
    srcs[1]
  }

  # React to source changes
  observeEvent(
    input$source_select,
    {
      req(input$source_select)
      message("[rlsquiz] Source changed to: ", input$source_select)
      rv$current_source <- input$source_select

      # Reset quiz state when changing sources
      if (opt_mode == "quiz") {
        rv$score <- 0L
        rv$total <- 0L
        rv$finished <- FALSE
      }

      if (build_bank()) {
        next_question()
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Status helpers
  show_status <- function(msg) {
    output$status_ui <- renderUI({
      div(
        style = "margin: 10px auto 0; max-width: 880px; font-weight:600; background:#fff; color:#000; padding:10px 14px; border-radius:10px;",
        msg
      )
    })
  }
  clear_status <- function() output$status_ui <- renderUI(NULL)

  # App state
  rv <- reactiveValues(
    bank = NULL,
    order = integer(0),
    idx = 0L,
    answered = FALSE,
    chosen_label = NULL,
    correct_label = NULL,
    options = NULL,
    current = NULL,
    score = 0L,
    total = 0L,
    target = if (is.na(opt_n)) Inf else as.integer(opt_n),
    finished = FALSE,
    current_source = NULL
  )

  # Build bank safely and report issues to the UI
  build_bank <- function() {
    clear_status()
    if (is.null(rv$current_source)) {
      show_status("No sources found under the photos directory.")
      return(FALSE)
    }
    items <- collect_items(base_dir, rv$current_source)
    if (nrow(items) == 0) {
      show_status(HTML(paste0(
        "No images found for source <b>",
        rv$current_source,
        "</b> in <code>",
        file.path(base_dir, rv$current_source),
        "</code>."
      )))
      return(FALSE)
    }
    n_species <- nrow(dplyr::distinct(items, species_key))
    if (n_species < 4) {
      show_status(HTML(paste0(
        "Need at least <b>4 species folders</b> in <code>",
        file.path(base_dir, rv$current_source),
        "</code>. Found ",
        n_species,
        "."
      )))
      return(FALSE)
    }

    rv$bank <- items
    rv$order <- sample(seq_len(nrow(items)))
    rv$idx <- 0L
    rv$answered <- FALSE
    rv$chosen_label <- NULL
    rv$correct_label <- NULL
    rv$options <- NULL
    rv$current <- NULL
    rv$score <- 0L
    rv$total <- 0L
    rv$finished <- FALSE
    TRUE
  }

  next_question <- function() {
    req(!is.null(rv$bank), nrow(rv$bank) > 0)
    if (opt_mode == "quiz" && rv$total >= rv$target) {
      rv$finished <- TRUE
      show_results()
      return(invisible(NULL))
    }
    rv$idx <- rv$idx + 1L
    if (rv$idx > length(rv$order)) {
      rv$order <- sample(seq_len(nrow(rv$bank)))
      rv$idx <- 1L
    }
    row <- rv$bank[rv$order[rv$idx], , drop = FALSE]
    rv$current <- row
    rv$answered <- FALSE
    rv$chosen_label <- NULL
    rv$correct_label <- row$species_lab

    all_species <- rv$bank |> distinct(species_key, species_lab)
    others <- all_species |>
      filter(species_key != row$species_key) |>
      slice_sample(n = 3)
    rv$options <- bind_rows(
      tibble(
        species_key = row$species_key,
        species_lab = row$species_lab,
        correct = TRUE
      ),
      mutate(others, correct = FALSE)
    ) |>
      slice_sample(n = 4)
  }

  # ---- INITIALISE inside a reactive context (this was the root cause) ----
  observeEvent(
    TRUE,
    {
      # pick source once
      rv$current_source <- choose_source()
      message(
        "[rlsquiz] Using source: ",
        rv$current_source %||% "<none>"
      )

      if (is.null(rv$current_source)) {
        show_status("No sources found under the photos directory.")
        return(invisible(NULL))
      }
      if (build_bank()) {
        next_question()
      }
    },
    once = TRUE,
    ignoreInit = FALSE
  )

  # ---- outputs ----
  output$image_ui <- renderUI({
    req(rv$current)
    src <- img_src(
      base_path_name = base_path_name,
      source = rv$current_source,
      species_key = rv$current$species_key,
      filename = rv$current$filename
    )
    tags$div(
      class = "img-wrap",
      tags$img(src = src, class = "flashcard-img", alt = "RLS habitat image")
    )
  })

  output$options_ui <- renderUI({
    req(rv$options)
    div(
      class = "options-grid",
      lapply(seq_len(nrow(rv$options)), function(i) {
        lab <- rv$options$species_lab[i]
        cls <- "opt-btn"
        if (rv$answered) {
          if (rv$options$correct[i]) {
            cls <- paste(cls, "is-correct")
          }
          if (!rv$options$correct[i] && identical(lab, rv$chosen_label)) {
            cls <- paste(cls, "is-wrong")
          }
        }
        actionButton(paste0("opt_", i), lab, class = cls)
      })
    )
  })

  # ---- interactions ----
  observe({
    req(rv$options)
    lapply(seq_len(nrow(rv$options)), function(i) {
      observeEvent(
        input[[paste0("opt_", i)]],
        {
          if (isTRUE(rv$answered) || isTRUE(rv$finished)) {
            return(NULL)
          }
          rv$answered <- TRUE
          rv$chosen_label <- rv$options$species_lab[i]
          if (isTRUE(rv$options$correct[i]) && opt_mode == "quiz") {
            rv$score <- rv$score + 1L
          }
        },
        ignoreInit = TRUE
      )
    })
  })

  observeEvent(input$next_btn, {
    if (isTRUE(rv$finished)) {
      return(invisible(NULL))
    }
    if (opt_mode == "quiz") {
      if (!isTRUE(rv$answered)) {
        return(invisible(NULL))
      }
      rv$total <- rv$total + 1L
    }
    next_question()
  })

  # HUD
  output$qnum <- renderText({
    if (opt_mode == "quiz") sprintf("%d / %d", rv$total + 1L, rv$target)
  })
  output$score_text <- renderText({
    if (opt_mode == "quiz") sprintf("%d correct", rv$score) else ""
  })

  # Results modal + optional CSV
  show_results <- function() {
    if (opt_mode != "quiz") {
      return(invisible(NULL))
    }
    percent <- round(100 * rv$score / rv$target)
    showModal(modalDialog(
      title = "Quiz complete",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        actionButton("restart", "Restart", class = "btn btn-primary")
      ),
      div(
        h3(sprintf("Score: %d / %d", rv$score, rv$target)),
        p(sprintf("That is %d%%", percent))
      )
    ))
    if (isTRUE(opt_save_csv)) {
      dir.create(opt_csv_path, recursive = TRUE, showWarnings = FALSE)
      out <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        user = if (is.null(opt_user)) NA_character_ else as.character(opt_user),
        source = rv$current_source,
        n = rv$target,
        correct = rv$score,
        percent = percent,
        stringsAsFactors = FALSE
      )
      fn <- file.path(
        opt_csv_path,
        sprintf(
          "rls_habitat_quiz_results_%s.csv",
          format(Sys.time(), "%Y%m%d_%H%M%S")
        )
      )
      try(utils::write.csv(out, fn, row.names = FALSE), silent = TRUE)
    }
  }

  observeEvent(input$restart, {
    removeModal()
    rv$score <- 0L
    rv$total <- 0L
    rv$finished <- FALSE
    rv$order <- sample(seq_len(nrow(rv$bank)))
    rv$idx <- 0L
    next_question()
  })
}

shinyApp(ui, server)
