
.First <- function() {
  # Set some warning/completion options.
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
  # Set our history file.
  if (interactive() && require(utils, quietly=TRUE)) {
    try(loadhistory(Sys.getenv('R_HISTFILE')))
  }
  # If there's a `.Rprofile` in this directory, let's source that,
  # too.
  if (interactive()) {
    base_r_profile = Sys.getenv('R_PROFILE_USER')
    if (nzchar(base_r_profile) && (getwd() != dirname(base_r_profile))) {
      local_r_profile = file.path(getwd(), '.Rprofile')
      if (file.exists(local_r_profile)) {
        source(local_r_profile, local = TRUE)
      }
    }
  }
}

.Last <- function() {
  if (interactive() && require(utils, quietly=TRUE)) {
    try(savehistory(Sys.getenv('R_HISTFILE')))
  }
}
