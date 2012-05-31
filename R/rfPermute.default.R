#' @rdname rfPermute
#' @usage \method{rfPermute}{default}(x, y, \dots, nrep = 100, clust.opts = NULL)

rfPermute.default <- function(x, y, ..., nrep = 100, clust.opts = NULL) {  
  # Takes same arguments as 'randomForest.default', plus
  #   'nrep': number of permutation replicates
  #   'clust.opts' : list of options to set up clusters for package 'parallel'
  #
  # Returns 'randomForest' object with:
  #   'null.dist' : 3 element named list with null distribution matrices
  #      for two importance types as first two elements 
  #      (columns are predictors, rows are permutation replicates),
  #      and 'pval' a matrix of p-values for each predictor (rows) on each
  #      importance metric (columns).
  #
  #  7/29/2011

  stopifnot(require(randomForest, quietly = TRUE))
  orig.call <- match.call()
  orig.call$nrep <- NULL
  orig.call$clust.opts <- NULL
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
  imp.names <- imp.names[c(length(imp.names) - 1, length(imp.names))]
  
  # permutes 'y' in rf.call 'nrep' times and runs randomForest
  permute.func <- function(nrep, rf.call) {
    lapply(1:nrep, function(i) {
      rf.call$y <- sample(rf.call$y)
      eval(rf.call)$importance
    })
  }
  
  # setup clusters if 'clust.opt' provided
  importance.perm <- if(!is.null(clust.opts)) {
    require(parallel, quietly = TRUE)
    
    clust <- if(is.vector(clust.opts) & length(clust.opts) == 1 & is.integer(clust.opts)) {
      makeCluster(clust.opts)
    } else if(is.list(clust.opts)) {
      do.call(makeCluster, clust.opts)
    } else {
      makeCluster(detectCores())
    }
    
    if(length(clust) == 1) { # if only one cluster was specified, do normal permutation
      stopCluster(clust)
      permute.func(nrep, rf.call)
    } else {
      clusterEvalQ(clust, library(randomForest))
      clusterSetRNGStream(clust)
      rep.split <- clusterSplit(clust, 1:nrep)
      result <- clusterApply(clust, sapply(rep.split, length), permute.func, rf.call = rf.call)
      stopCluster(clust) 
      do.call(c, result)    
    }
  } else {
    permute.func(nrep, rf.call)
  }
    
  # create null distribution for each variable  
  rf$null.dist <- sapply(imp.names, function(imp.type) {
    t(sapply(1:length(importance.perm), function(i) importance.perm[[i]][, imp.type]))
  }, simplify = FALSE)
  
  # calculate p-value of observed importance metrics
  rf$null.dist$pval <- t(sapply(rownames(rf$importance), function(pred) {
    result <- sapply(names(rf$null.dist), function(imp.type) {
      num.perm.gte <- sum(rf$null.dist[[imp.type]][, pred] >= rf$importance[pred, imp.type]) + 1
      num.perm.gte / (nrow(rf$null.dist[[imp.type]]) + 1)
    }, USE.NAMES = TRUE)
  }, USE.NAMES = TRUE))
  
  rf$call <- orig.call
  class(rf) <- c("rfPermute", "randomForest")
  return(rf)  
}

