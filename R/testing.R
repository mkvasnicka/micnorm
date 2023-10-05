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


#' Check if two queries to IS gives the same normalized data.
#'
#' This function checks if two queries to IS give the same data.
#' As IS returns the data in different order, the data frames are
#' sorted by \code{student_uco} before comparison.
#'
#' @param x tibble returned by \code{\link{normalize_micro}}
#' @param y tibble returned by \code{\link{normalize_micro}}
#' @return A logical value indicating whether the two tibbles are the same
#' @examples
#' res <- normalize_micro(
#'   micro,
#'   mivs,
#'   course_mapping = mivsmicro,
#'   export_to_IS = FALSE,
#'   send_mail = FALSE
#' )
#' res2 <- normalize_micro(
#'   micro,
#'   mivs,
#'   course_mapping = mivsmicro,
#'   export_to_IS = FALSE,
#'   send_mail = FALSE
#' )
#' is_same(df1, df2) # should return TRUE
is_same <- function(x, y) {
  all.equal(dplyr::arrange(x, student_uco), dplyr::arrange(y, student_uco))
}
