.cc_pedantic <- function() {
  if (is.null(getOption("cc_pedantic"))) {
    message("Turning on pedantic warnings\n")
    options(
        cc_pedantic = TRUE,
        warnPartialMatchArgs = TRUE,
        warnPartialMatchAttr = TRUE,
        warnPartialMatchDollar = TRUE,
        warn = 1
    )
  } else {
    message("Turning off pedantic warnings\n")
    options(
        cc_pedantic = NULL,
        warnPartialMatchArgs = FALSE,
        warnPartialMatchAttr = FALSE,
        warnPartialMatchDollar = FALSE,
        warn = 0
    )
  }
}

.First <- function() {
  # Set some warning/completion options.
  options(repos = c(CRAN = "https://cran.rstudio.com"),
          useFancyQuotes = FALSE,
          menu.graphics = FALSE,
          deparse.max.lines = 2,
          browserNLdisabled = TRUE)
  utils::rc.settings(ipck = TRUE)
  # Definitely turn up jupyter logging.
  options(jupyter.log_level = 3)
  # Debugging FTW.
  options(error = traceback)
  # Set our history file.
  if (interactive() && require(utils, quietly=TRUE)) {
    histfile <- Sys.getenv("R_HISTFILE", unset = path.expand("~/.Rhistory"))
    try(loadhistory(histfile))
  }
  # If there's a `.Rprofile` in this directory, let's source that,
  # too.
  if (interactive()) {
    base_r_profile <- Sys.getenv("R_PROFILE_USER")
    if (nzchar(base_r_profile) && (getwd() != dirname(base_r_profile))) {
      local_r_profile <- file.path(getwd(), ".Rprofile")
      if (file.exists(local_r_profile)) {
        source(local_r_profile, local = TRUE)
      }
    }
  }
}

.Last <- function() {
  if (interactive() && require(utils, quietly=TRUE)) {
    histfile <- Sys.getenv("R_HISTFILE", unset = path.expand("~/.Rhistory"))
    try(savehistory(histfile))
  }
}
