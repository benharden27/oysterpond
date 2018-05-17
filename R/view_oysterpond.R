#' Run the oysterpond shiny interface
#'
#' @return
#' @export
#'
#' @examples
view_oysterpond <- function() {
  appDir <- system.file("shiny-app", "app", package = "oysterpond")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `oysterpond`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
