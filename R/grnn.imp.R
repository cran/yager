#' Derive the importance rank of all predictors used in the GRNN 
#'
#' The function \code{grnn.imp} derives the importance rank of all predictors used in the GRNN
#' It essentially is a wrapper around the function \code{grnn.x_imp}.
#'
#' @param net   The GRNN object generated by grnn.fit() 
#' @param class TRUE or FALSE, whether it is for the classification or not
#'
#' @return A dataframe with important values of all predictors in the GRNN
#'
#' @seealso \code{\link{grnn.x_imp}}
#'
#' @examples
#' data(iris, package = "datasets")
#' Y <- ifelse(iris[, 5] == "setosa", 1, 0)
#' X <- scale(iris[, 1:3])
#' gnet <- grnn.fit(x = X, y = Y)
#' \dontrun{
#' grnn.imp(net = gnet, class = TRUE)
#' }

grnn.imp <- function(net, class = FALSE) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (!(class %in% c(TRUE, FALSE))) stop("the class input is not correct.", call. = F)

  cls <- parallel::makeCluster(min(ncol(net$x), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "class", "grnn.fit", "grnn.predone", "grnn.predict", "grnn.x_imp")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst1 <- data.frame(idx = seq(ncol(net$x)),
                     Reduce(rbind, parallel::parLapply(cls, seq(ncol(net$x)), function(i) grnn.x_imp(net, i, class = class))))
  parallel::stopCluster(cls)
  rst2 <- rst1[with(rst1, order(-imp1, -imp2)), ]
  row.names(rst2) <- NULL
  return(rst2)
}

