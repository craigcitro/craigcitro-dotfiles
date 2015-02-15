
.First <- function() {
  options(repos = c(CRAN = "http://cran.rstudio.com"),
          warnPartialMatchArgs = TRUE,
          warnPartialMatchAttr = TRUE,
          warnPartialMatchDollar = TRUE,
          warn = 1,
          useFancyQuotes = FALSE,
          menu.graphics = FALSE,
          deparse.max.lines = 2,
          browserNLdisabled = TRUE)
  utils::rc.settings(ipck = TRUE)
  if (interactive() && require(utils, quietly=TRUE)) {
    try(loadhistory(Sys.getenv('R_HISTFILE')))
  }
}

.Last <- function() {
  if (interactive() && require(utils, quietly=TRUE)) {
    try(savehistory(Sys.getenv('R_HISTFILE')))
  }
}
