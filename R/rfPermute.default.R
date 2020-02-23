#' @rdname rfPermute
#' 
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
    
    call.x <- x
    # Get importance scores for permutations
    #  a list of 3-dimensional arrays of importance scores
    cl <- swfscMisc::setupClusters(num.cores)
    null.dist <- tryCatch({
      if(is.null(cl)) { # Don't parallelize if num.cores == 1      
        lapply(ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call)
      } else { # Run random forest using parLapply
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(
          cl = cl, 
          varlist = c("call.x", "rf.call"), 
          envir = environment()
        )
        parallel::parLapplyLB(
          cl, ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call
        )
      }
    }, finally = if(!is.null(cl)) parallel::stopCluster(cl) else NULL)
    
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