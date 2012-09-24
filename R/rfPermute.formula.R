#' @rdname rfPermute
#' @usage \method{rfPermute}{formula}(formula, data = NULL, \dots, subset, na.action = na.fail, nrep = 100, num.cores = options("mc.cores"))

rfPermute.formula <- function(formula, data = NULL, ..., subset, na.action = na.fail, nrep = 100, num.cores = options("mc.cores")) {
  # Takes same arguments as 'randomForest.formula', plus
  #   'nrep': number of permutation replicates
  #   'num.cores' : number of cores to use
  #
  # Returns 'randomForest' object with:
  #   'null.dist' : 3 element named list with null distribution matrices
  #      for two importance types as first two elements 
  #      (columns are predictors, rows are permutation replicates),
  #      and 'pval' a matrix of p-values for each predictor (rows) on each
  #      importance metric (columns).
  #
  #  9/20/2012

  stopifnot(require(randomForest, quietly = TRUE))
  if (!inherits(formula, "formula")) stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (any(c("xtest", "ytest") %in% names(m))) stop("xtest/ytest not supported through the formula interface")
  
  # extract formula terms
  names(m)[2] <- "formula"
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m$... <- NULL
  m$nrep <- NULL
  m$num.cores <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  y <- model.response(m)
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)), data.frame(m))
  for (i in seq(along = ncol(m))) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  
  # run rfPermute
  rf.call <- match.call()
  rf.call[[1]] <- as.name("rfPermute")
  names(rf.call)[2:3] <- c("x", "y")
  rf.call$x <- m
  rf.call$y <- y
  rf.call$subset <- rf.call$na.action <- NULL
  rf.call[-1] <- lapply(rf.call[-1], eval, envir = parent.frame())
  rf <- eval(rf.call)
  
  # reconstitute original randomForest call
  rf.call <- match.call()
  rf.call[[1]] <- as.name("randomForest")
  rf$call <- rf.call
  rf$call$nrep <- NULL
  rf$call$num.cores <- NULL
  rf$terms <- Terms
  if (!is.null(attr(m, "na.action"))) rf$na.action <- attr(m, "na.action")
  
  class(rf) <- c("rfPermute", "randomForest.formula", "randomForest")
  return(rf)
}

