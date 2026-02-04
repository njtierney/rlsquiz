#' Launch the RLS Habitat Quiz in practice mode
#'
#' @param source Optional name of the image source folder (e.g. "rls_catalogue").
#' @param photos_root Optional external path overriding inst/app/www/photos.
#'
#' @export
rls_practice <- function(source = NULL, photos_root = NULL) {
  # Use local app in development (inst/app)
  local_app <- file.path("inst", "app")
  if (file.exists(file.path(local_app, "app.R"))) {
    app_dir <- normalizePath(local_app)
  } else {
    # Fallback to installed package location once you install it
    app_dir <- system.file("app", package = "rls.habitat.quiz")
    if (app_dir == "") {
      stop(
        "Could not find app in inst/app or in the installed package 'rls.habitat.quiz'"
      )
    }
  }

  # Pass defaults to the app (using NEW option names)
  options(rlsquiz.mode = "practice")
  options(rlsquiz.default_source = source)
  options(rlsquiz.photos_root = photos_root)

  shiny::runApp(app_dir, launch.browser = TRUE)
}
