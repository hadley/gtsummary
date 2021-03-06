#' The gtsummary logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @md
#' @export
#' @examples
#' gtsummary_logo()
gtsummary_logo <- function(unicode = l10n_info()$`UTF-8`) {
  # get the letters at https://www.messletters.com/en/big-text/
  # font is smslant
  logo <-
    c(
      "   0      __  1            2               3    4",
      "    ___ _/ /_ ___ __ ____ _  __ _  ___ _______ __",
      " 9 / _ `/ __/(_-</ // /  ' \\/  ' \\/ _ `/ __/ // /",
      "   \\_, /\\__//___/\\_,_/_/_/_/_/_/_/\\_,_/_/  \\_, / ",
      "  /___/  5        6       7     8        9/___/  ",
      "                                                 "
    )

  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
  if (unicode) hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

  cols <- c(
    "red", "yellow", "green", "magenta", "cyan",
    "yellow", "green", "white", "magenta", "cyan"
  )

  col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(crayon::blue(logo), class = "gtsummary_logo")
}

#' @export

print.gtsummary_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
