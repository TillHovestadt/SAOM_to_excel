#' Converts sienaBayesFit objects to a Data.frames for convenient Storing of results
#'
#' @param sienaBayesFitObject The sienaBayesFit object to utilize
#' @param writeToExcel Logical. Should the data.frame also be stored to an Excel File? Defaults to False
#' @param writePath Character. Path where the Excel table is going to be stored. Defaults to NULL, in which case the table is stored in the current working directory (if writeToExcel == T)
#' @param writeName Character. Name of the Excel table. Defaults to NULL, in which case the table will be named "model1.xlsx" (if writeToExcel == T). Table name does not have to include ".xlsx" -ending
#' @return A Data.Frame of the Name, Estimate, Posterior SE, lower and upper credibility interval limits, t-Value (Estimate / Posterior SE) and significance value (abs(t-Value) > 1.96?)
#' @export
sienaBayesResults <- function(sienaBayesFitObject,
                        writeToExcel = F,
                        writePath = NULL,
                        writeName = NULL) {
  # make sure that only "sienaBayesFit" objects are passed on

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
      "writexl",
      "coda"
    )
  )


  # save a data.frame with the results
  tempresults <- RSienaTest::shortBayesResults(sienaBayesFitObject)

  # extract the mcmc chains of the results for p-values
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
  modelparam <- mcmcchainsarray[-grep(pattern = "rate", x = colnames(mcmcchainsarray))]
  modelparam <- modelparam[-grep(pattern = ".1", x = colnames(modelparam))]

  # modelparam now contains the mcmc chains for all the parameters of interest in the SAOM model

  postprobs <- data.frame(
    name = colnames(modelparam),
    p = NA)

  for (i in colnames(modelparam)) {

    chain <- coda::as.mcmc(modelparam[[i]])

    assign(x = paste0(i, "_chain"),
           value = chain)

    postprob <- sum(get(paste0(i, "_chain")) > 0) / length(get(paste0(i, "_chain")))

    postprobs$p[postprobs$name == i] <- postprob


  }
# postprobs now contains all the posterior p-values we need

  # reduce the datafram to the interesting variables
  reducedtempres <- subset(
    x = tempresults,
    subset = !(effectName %in% c(
      grep(
        pattern = "period",
        x = tempresults$effectName,
        value = TRUE
      )
    )),
    select = c(effectName,
               postMeanGlobal,
               postSdGlobal,
               cFrom,
               cTo)
  )

  # rename the data.frame
  resultsdataframe <- data.frame(
    "name" = reducedtempres$effectName,
    "estimate" = reducedtempres$postMeanGlobal,
    "se" = reducedtempres$postSdGlobal,
    "cFrom" = reducedtempres$cFrom,
    "cTo" = reducedtempres$cTo,
    "tValue" = reducedtempres$postMeanGlobal / reducedtempres$postSdGlobal
  )

  # attach p-values to dataframe
  resultsdataframe <- merge(x = resultsdataframe,
                            y = postprobs,
                            by = "name")

  if (writeToExcel == T) {
    if (is.null(writePath)) {
      # if no path to write to is supplied, write to working directory

      if (is.null(writeName)) {
        write_xlsx(x = resultsdataframe,
                   path = paste0("model1.xlsx"))

      } else
        (if (length(grep(
          pattern = ".xlsx",
          x = writeName,
          value = T,
          fixed = T
        )) > 0 ) {
          write_xlsx(x = resultsdataframe,
                     path = paste0(writeName))

        } else
          (write_xlsx(x = resultsdataframe,
                      path = paste0(writeName, ".xlsx")))
        )

    } else
      (if (is.null(writeName)) {
        write_xlsx(x = resultsdataframe,
                   path = paste0(writePath, "/", "model1.xlsx"))

      } else
        (if (length(grep(
          pattern = ".xlsx",
          x = writeName,
          value = T,
          fixed = T
        )) > 0 ) {
          write_xlsx(x = resultsdataframe,
                     path = paste0(writePath, "/", writeName))

        } else
          (write_xlsx(x = resultsdataframe,
                      path = paste0(writePath,"/", writeName, ".xlsx")))
        ))

  }

  return(resultsdataframe)

}
