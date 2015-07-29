#' @rdname rfPermute
#' 
#' @importFrom parallel makeForkCluster parLapply stopCluster
#' @export rfPermute.default
#' @export
#' 
rfPermute.default <- function(x, y, ..., nrep = 100, num.cores = 1) {  
  orig.call <- match.call()
  orig.call$nrep <- NULL
  orig.call$num.cores <- NULL
  orig.call[[1]] <- as.name("randomForest")
  rf.call <- orig.call
  rf.call$x <- x
  rf.call$y <- y
  imp.element <- pmatch(names(rf.call), "importance")
  if(!all(is.na(imp.element))) {
    imp.id <- which(!is.na(imp.element))
    names(rf.call)[imp.id] <- "importance"
  }    
  rf.call$importance <- TRUE
  rf.call[-1] <- lapply(as.list(rf.call[-1]), eval, envir = parent.frame())
  rf <- eval(rf.call)
  imp.names <- colnames(rf$importance)
  
  # permutes 'y' in rf.call 'nrep' times and runs randomForest  
  if(nrep > 0) {
    # define permutation function
    permFunc <- function(y, perm.rf.call) {
      rf.call$y <- y
      eval(rf.call)$importance
    }
    
    # create list of permuted y values
    ran.y <- lapply(1:nrep, function(i) sample(rf.call$y))
    
    # get importance scores for permutations
    importance.perm <- NULL
    if(num.cores > 1) {
      cl <- makeForkCluster(num.cores)
      tryCatch({
        importance.perm <- parLapply(cl, ran.y, permFunc, perm.rf.call = rf.call)
      }, finally = stopCluster(cl))
    } else {
      importance.perm <- lapply(ran.y, permFunc)
    }
    
    # create null distribution for each variable  
    rf$null.dist <- sapply(imp.names, function(imp.type) {
      null.dist <- sapply(1:length(importance.perm), function(i) importance.perm[[i]][, imp.type, drop = FALSE])
      null.dist <- t(rbind(null.dist))
      colnames(null.dist) <- rownames(rf$importance)
      null.dist
    }, simplify = FALSE)
    
    # calculate p-value of observed importance metrics
    rf$null.dist$pval <- calc.imp.pval(rf, imp.names) 
  } 
  
  rf$call <- orig.call
  class(rf) <- c("rfPermute", "randomForest")
  return(rf)  
}