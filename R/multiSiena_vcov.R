#' Calculates posterior covariance matrix  of sienaBayesFit parameters
#'
#' @param sienaBayesFitObject The sienaBayesFit object to utilize
#' @param excludeRate Whether rate parameters should be excluded, defaults to true
#' @return posterior covariance matrix
#' @export
sienaBayesvcov <- function(sienaBayesFitObject,
                           excludeRate = T){

  stopifnot(class(sienaBayesFitObject) %in% c("sienaBayesFit"))

  if (!require("pacman")) {
    if (!require("devtools")) {
      install.packages("devtools", dep = T)
    }
    library(devtools)
    install_github("TillHovestadt/Pacman_R_Package")
    library(pacman)
  }

  pacman(
    name = c(
      "RSiena",
      "RSienaTest",
      "multiSiena",
      "coda"
    )
  )

  # save a data.frame with the results
  tempresults <- RSienaTest::shortBayesResults(sienaBayesFitObject)

  # extract the mcmc chains of the results for variance covariance matrix
  mcmcchainsarray <-
    as.data.frame(x = multiSiena::extract.sienaBayes(zlist = list(sienaBayesFitObject),
                                                     extracted = "all"))

  ## get rid of the "chain 1.", mu" or " log(s.d.)" endings on some of the variable names

  b <- strsplit(x = variable.names(mcmcchainsarray),
                split = "chain 1.")
  newnames <- c()
  for (i in 1:length(variable.names(mcmcchainsarray))) {

    newnames[i] <- b[[i]][2]

  }
  b <- strsplit(x = newnames,
                split = " mu")
  for (i in 1:length(newnames)) {

    newnames[i] <- b[[i]][1]

  }

  b <- strsplit(x = newnames,
                split = " log(s.d.)",
                fixed = T)
  for (i in 1:length(newnames)) {

    newnames[i] <- b[[i]][1]

  }
  colnames(mcmcchainsarray) <- newnames

  if (excludeRate == T) {
    mcmcchainsarray <- mcmcchainsarray[-grep(pattern = "rate", x = colnames(mcmcchainsarray))]
    mcmcchainsarray <- mcmcchainsarray[-grep(pattern = ".1", x = colnames(mcmcchainsarray))]
    }


  mcmcchainsmatrix <- as.matrix(mcmcchainsarray)

  post_cov_matrix <- cov(mcmcchainsmatrix)

  return(post_cov_matrix)

}
