#' Launch the RLS Habitat ID practice app
#'
#' Reads images from bundled `inst/app/www/photos/<source>/<species>/...`
#' or from an external `photos_root` with the same structure.
#' @param source Default location/project folder to use (e.g., "rls_catalogue")
#' @param photos_root Optional external folder containing sources and species subfolders
#' @export
rls_practice <- function(source = NULL, photos_root = NULL) {
  run_rls_app(
    mode = "practice",
    source = source,
    n = NA_integer_,
    photos_root = photos_root,
    save_csv = FALSE,
    csv_path = NULL,
    user = NULL
  )
}

#' Launch a graded RLS quiz
#'
#' Presents n questions and reports a score. Optionally writes a CSV summary.
#' @param source Location/project folder (e.g., "rls_catalogue")
#' @param n Number of questions (default 20)
#' @param photos_root Optional external folder of images
#' @param save_csv Write a CSV summary at the end
#' @param csv_path Directory to write the CSV into
#' @param user Optional user name recorded in the CSV
#' @export
rls_quiz <- function(
  source = NULL,
  n = 20,
  photos_root = NULL,
  save_csv = FALSE,
  csv_path = getwd(),
  user = Sys.info()[["user"]]
) {
  stopifnot(length(n) == 1L, is.numeric(n), n >= 1)

  run_rls_app(
    mode = "quiz",
    source = source,
    n = as.integer(n),
    photos_root = photos_root,
    save_csv = isTRUE(save_csv),
    csv_path = csv_path,
    user = user
  )
}

# Internal helper -------------------------------------------------------------

run_rls_app <- function(
  mode,
  source = NULL,
  n = NA_integer_,
  photos_root = NULL,
  save_csv = FALSE,
  csv_path = NULL,
  user = NULL
) {
  # Look for app inside your package folder
  app_dir_local <- file.path("inst", "app")
  if (file.exists(file.path(app_dir_local, "app.R"))) {
    app_dir <- normalizePath(app_dir_local, mustWork = TRUE)
  } else {
    app_dir <- system.file("app", package = "rlsquiz")
  }

  if (!nzchar(app_dir)) {
    stop("Could not locate Shiny app in inst/app or in the installed package.")
  }

  # Pass run-time options to the app
  options(
    rlsquiz.mode = mode,
    rlsquiz.default_source = source,
    rlsquiz.n = n,
    rlsquiz.photos_root = photos_root,
    rlsquiz.save_csv = save_csv,
    rlsquiz.csv_path = csv_path,
    rlsquiz.user = user
  )

  shiny::runApp(app_dir, launch.browser = TRUE)
}
