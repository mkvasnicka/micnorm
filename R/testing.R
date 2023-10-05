#' View Output
#'
#' This function is used to view the output string of Microeconomics 1
#' normalization.
#'
#' @param tab tibble provided by \code{\link{normalize_micro}}.
#' @param semin vector of seminars to be viewed provided as strings,
#'  e.g., `"04"`.
#' @param by_semin logical, if \code{TRUE}, the output is sorted by
#'  seminar first, and then by last names; \code{TRUE} is default
#'  value.
#' @return None
#'
#' @examples
#' view_output(tab = my_table)
#' view_output(tab = my_table, semin = c("04", "06"))
view_output <- function(tab, semin = NULL, by_semin = TRUE) {
  if (is.numeric(semin)) {
    semin <- stringr::str_pad(semin, 2, pad = "0")
  }
  if (!is.null(semin)) {
    tab <- dplyr::filter(tab, seminar %in% semin)
  }
  tab |>
    dplyr::select(
      seminar,
      student_uco,
      student_last_name,
      student_first_name,
      output_string
    ) |>
    dplyr::mutate(
      view = stringr::str_c(
        "#", seminar, ": ",
        student_last_name, " ",
        student_first_name,
        " (", student_uco,")\n",
        output_string
      )
    ) |>
    dplyr::arrange(
      if (by_semin) seminar,
      student_last_name,
      .locale = "cs"
    ) |>
    dplyr::pull(view) |>
    cat(sep = "\n---\n\n")
}
