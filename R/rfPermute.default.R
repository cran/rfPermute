#' @rdname rfPermute
#' 
#' @export rfPermute.default
#' @export
#' 
rfPermute.default <- function(x, y, ..., nrep = 100, num.cores = NULL) {  
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
  rf$call <- orig.call
  
  # permutes 'y' in rf.call 'nrep' times and runs randomForest  
  if(nrep > 0) {
    # Setup number of cores
    if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
    if(is.na(num.cores)) num.cores <- 1
    num.cores <- max(1, num.cores)
    num.cores <- min(parallel::detectCores() - 1, num.cores)
    
    # Create list of permuted y values
    ran.y <- lapply(1:nrep, function(i) sample(rf.call$y))
    
    # Get importance scores for permutations
    #  a list of 3-dimensional arrays of importance scores
    null.dist <- if(num.cores == 1) {
      # Don't use parallelizing if num.cores == 1      
      lapply(ran.y, .permFunc, x = x, perm.rf.call = rf.call)
    } else if(Sys.info()[["sysname"]] %in% c("Linux", "Darwin")) {
      # Run random forest on Linux or Macs using mclapply
      parallel::mclapply(
        ran.y, .permFunc, x = x, perm.rf.call = rf.call, mc.cores = num.cores
      )
    } else {
      # Run random forest using parLapply
      tryCatch({
        cl <- parallel::makeCluster(num.cores)
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(cl, "x", "rf.call", environment())
        parallel::parLapply(cl, ran.y, .permFunc, x = x, perm.rf.call = rf.call)
      }, finally = parallel::stopCluster(cl))
    }
    
    # create and load null distribution arrays for each scaled and unscaled importances
    rf$null.dist <- list(unscaled = .makeImpArray(rf$importance, nrep, NULL))
    rf$null.dist$scaled <- rf$null.dist$unscaled
    for(i in 1:nrep) {
      rf$null.dist$unscaled[, , i] <- null.dist[[i]][, , "unscaled"]
      rf$null.dist$scaled[, , i] <- null.dist[[i]][, , "scaled"]
    }
    
    # calculate p-value of observed importance metrics
    rf$pval <- .calcImpPval(rf) 
    
    class(rf) <- c("rfPermute", "randomForest")
  }
  
  return(rf)  
}