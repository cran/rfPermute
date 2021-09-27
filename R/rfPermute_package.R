#' @docType package
#' @name rfPermute_package 
#' @title rfPermute model object
#' @description Container for rfPermute results
#' 
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom plyr .
#' @keywords package
#' 
#' @export
rfPermuteTutorial <- function() {
  utils::browseURL(system.file("rfPermute_Tutorial.html", package = "rfPermute"))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to rfPermute v", utils::packageVersion("rfPermute"), "\n",
    "See rfPermuteTutorial() for a guide to the package."
  )
}
