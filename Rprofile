
.First <- function() {
  options(repos = c(CRAN = "http://cran.rstudio.com"),
          menu.graphics = FALSE,
          deparse.max.lines = 2,
          browserNLdisabled = TRUE)
}

.Last <- function() {
  if (interactive()) {
    try(savehistory("~/.Rhistory"))
  }
}

if (interactive()) {
  options(warn = -1)
  require(devtools, quietly = TRUE)
  options(warn = 0)
}
