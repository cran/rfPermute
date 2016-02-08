#' @title Combine rfPermute Objects
#' @description Combines two or more ensembles of \code{rfPermute} objects into one, combining
#' \code{randomForest} results, null distributions, and re-calculating p-values.
#' 
#' @param \dots two or more objects of class \code{rfPermute}, to be combined into one.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[randomForest]{combine}}
#' 
#' @examples
#' 
#' data(iris)
#' rp1 <- rfPermute(Species ~ ., iris, ntree=50, norm.votes=FALSE, nrep = 100)
#' rp2 <- rfPermute(Species ~ ., iris, ntree=50, norm.votes=FALSE, nrep = 100)
#' rp3 <- rfPermute(Species ~ ., iris, ntree=50, norm.votes=FALSE, nrep = 100)
#' rp.all <- rp.combine(rp1, rp2, rp3)
#' plot(rp.all)
#' 
#' @importFrom randomForest combine
#' @export
#' 
rp.combine <- function(...) {
  rp.list <- list(...)
  are.rp <- sapply(rp.list, function(x) inherits(x, c("rfPermute", "randomForest")))
  if(any(!are.rp)) stop("Argument must be a list of rfPermute objects.")
  
  rf <- do.call(combine, rp.list)
  imp.types <- colnames(rf$importance)
  rf$null.dist <- sapply(imp.types, function(imp) {
    do.call(rbind, lapply(rp.list, function(rp) rp$null.dist[[imp]]))
  }, simplify = FALSE)
  rf$null.dist$pval <- calc.imp.pval(rf, imp.types)
  
  class(rf) <- c("rfPermute", "randomForest")
  return(rf)  
}