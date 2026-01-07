# ============================================================================
# MOMACS WRAPPER FUNCTION
# ============================================================================

#' Launch Momacs Application
#'
#' Starts the Shiny application for morphometric digitization of images.
#' Supports landmarks, polylines, polygons, curves, and closed outlines.
#'
#' @param folder_path Optional path to folder containing images to digitize.
#'   If NULL, use the Browse button in the app to select a folder.
#'
#' @return Invisibly returns the Shiny app object
#'
#' @examples
#' \dontrun{
#' # Launch with folder selection in the app
#' momacs()
#'
#' # Launch with a specific folder
#' momacs("~/my_images")
#' }
#'
#' @export
momacs <- function(folder_path = NULL) {
  # Set up resource path for static files
  app_dir <- system.file("app", package = "momacs")
  if (app_dir == "") {
    stop("Could not find app directory. Is momacs installed correctly?")
  }

  www_dir <- file.path(app_dir, "www")
  if (dir.exists(www_dir)) {
    shiny::addResourcePath("momx", www_dir)
  }

  if (is.null(folder_path)) {
    message("No folder specified. Use Browse button to select a folder.")
  } else {
    if (!dir.exists(folder_path)) {
      stop("Folder does not exist: ", folder_path)
    }
    message("Starting Momacs with folder: ", folder_path)
    # Note: folder_path can be accessed via function argument or through UI folder browser
  }

  app <- shinyApp(ui, server, options = list(launch.browser = TRUE))
  invisible(app)
}
