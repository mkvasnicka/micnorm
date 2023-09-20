# errors and warnings logging -------------------------------------------------

no_of_errors <- 0
no_of_warnings <- 0



# credentials -----------------------------------------------------------------

#' @export 
#' @importFrom MUIS credentials
MUIS::credentials



# mailing ---------------------------------------------------------------------

#' Send email
#'
#' `send_mail()` sends an mail from my school mail account to my school mail
#' account
#'
#' @param subject (character string) mail header
#' @param body (character string nebo MIME) mail body (can include \n)
#' @param sender (character string) sender's email address; implicitly mine
#' @param recipient (character string) recipient's email address;
#'  implicitly the same as sender's one
#'
#' @return none -- it just sends a mail
#'
#' @details there must be sendmail present on the server
send_mail <- function(
    subject,
    body,
    sender = "847@muni.cz",
    recipients = sender) {
  logging::loginfo("Sending the mail.")
  try({
    email <- mailR::send.mail(
      from = sender,
      to = recipients,
      subject = subject,
      body = body,
      smtp = list(host.name = "relay.muni.cz", port = 25),
      authenticate = FALSE,
      send = FALSE
    )
    email$send()
  })
}



# students and teachers -------------------------------------------------------

#' Get list of all students.
#'
#' `get_all_students()` downloads a list of all students within all seminars of
#' several courses
#'
#' @param `...` credentials; separated by commas
#'
#' @return a tibble of all students; it includes columns:
#' - course,
#' - seminar,
#' - student_uco,
#' - student_last_name,
#' - student_first_name, and
#' - credentials
#'
#' @details it is protected against errors; it logs
#'
#' @examples \dontrun{
#' students <- get_all_students(micprez, mivs)
#' }
get_all_students <- function(...) {
  creds <- list(...)
  logging::loginfo(
    "Trying to download list of students in seminars in %s courses.",
    length(creds)
  )
  tryCatch(
    {
      purrr::map(creds, function(c) {
        logging::loginfo(
          "... downloading students in course %s.",
          c$course
        )
        tryCatch(
          MUIS::get_seminar_students(c),
          error = function(e) {
            logging::logerror(e)
            no_of_errors <<- no_of_errors + 1
            NULL
          }
        )
      }) |>
        dplyr::bind_rows() |>
        dplyr::select(
          course,
          seminar,
          student_uco = uco,
          student_last_name = last_name,
          student_first_name = first_name,
          credentials = credentials
        )
    },
    error = function(e) {
      logging::logerror(e)
      no_of_errors <<- no_of_errors + 1
      NULL
    }
  )
}


#' Get list of all teachers.
#'
#' `get_all_teachers()` downloads a list of all teachers within several courses
#'
#' @param `...` credentials; separated by commas
#'
#' @return a tibble of all teachers; it includes columns:
#' - course,
#' - seminar,
#' - teacher_uco,
#' - teacher_last_name, and
#' - teacher_first_name
#'
#' @details it is protected against errors; it logs
#'
#' @examples \dontrun{
#' teachers <- get_all_teachers(micprez, mivs)
#' }
get_all_teachers <- function(...) {
  creds <- list(...)
  logging::loginfo(
    "Trying to download list of all teachers in %s courses.",
    length(creds)
  )
  tryCatch(
    {
      purrr::map(creds, function(c) {
        logging::loginfo(
          "... downloading teachers in course %s.",
          c$course
        )
        tryCatch(
          MUIS::get_teachers(c),
          error = function(e) {
            logging::logerror(e)
            no_of_errors <<- no_of_errors + 1
            NULL
          }
        )
      }) |>
        dplyr::bind_rows() |>
        dplyr::select(
          course,
          seminar,
          teacher_uco = uco,
          teacher_last_name = last_name,
          teacher_first_name = first_name,
          credentials = credentials
        )
    },
    error = function(e) {
      logging::logerror(e)
      no_of_errors <<- no_of_errors + 1
      NULL
    }
  )
}



# seminar points --------------------------------------------------------------

#' Get list of all seminar points notebooks.
#' 
#' `get_names_of_all_existings_blocks()` downloads names of all existing blocks
#' within several courses
#'
#' @param `...` credentials; separated by commas
#'
#' @return a tibble of names of all existing blocks; it includes columns:
#' - course,
#' - name,
#' - shortcut, and
#' - credential
#'
#' @details it is protected against errors; it logs
#'
#' @examples \dontrun{
#' blocks <- get_names_of_all_existings_blocks(micprez, mivs)
#' }
get_names_of_all_existings_blocks <- function(...) {
  creds <- list(...)
  logging::loginfo(
    "Trying to download names of all existing blocks in %s courses.",
    length(creds)
  )
  tryCatch(
    {
      purrr::map(creds, function(c) {
        logging::loginfo(
          "... downloadint names of blocks in course %s.",
          c$course
        )
        tryCatch(
          MUIS::list_notebooks(c),
          error = function(e) {
            logging::logerror(e)
            no_of_errors <<- no_of_errors + 1
            NULL
          }
        )
      }) |>
        dplyr::bind_rows() |>
        dplyr::filter(stringr::str_detect(shortcut, "^bodysemin\\d{2}")) |>
        dplyr::arrange(shortcut, course)
    },
    error = function(e) {
      logging::logerror(e)
      no_of_errors <<- no_of_errors + 1
      NULL
    }
  )
}


#' Read studnets' points from seminar notebooks.
#' `read_points_from_blocks()` reads all blocks found by
#' get_names_of_all_existings_blocks() and returns students' points stored there
#'
#' @param blocks a tibble returned by `get_names_of_all_existings_blocks()`
#'
#' @return a tibble with columns:
#' - uco,
#' - points,
#' - course, and
#' - block
read_points_from_blocks <- function(blocks) {
  logging::loginfo(
    "Trying to download points in %s blocks.",
    nrow(blocks)
  )
  tryCatch(
    {
      purrr::map2(
        blocks$credentials,
        blocks$shortcut,
        function(cred, shortcut) {
          logging::loginfo(
            "... downloading block %s in course %s.",
            shortcut, cred$course
          )
          MUIS::read_notebook(cred, shortcut)
        }
      ) |>
        dplyr::bind_rows()
    },
    error = function(e) {
      logging::logerror(e)
      no_of_errors <<- no_of_errors + 1
      NULL
    }
  )
}


# renegades -------------------------------------------------------------------

#' Get list of students that stopped studying.
#'
#' `get_renegades()` downloads names of all renegades (i.e. students that
#' stopped working within several courses.
#'
#' @param `...` credentials; separated by commas
#'
#' @return an integer vectors including UCOs of all renegades
#'
#' @details it is protected against errors; it logs
#'
#' @examples \dontrun{
#' renegades <- get_renegades(micprez, mivs)
#' }
get_renegades <- function(...) {
  empty <- tibble::tibble(uco = integer(0), content = character(0))
  creds <- list(...)
  logging::loginfo(
    "Trying to download the list of renegades in %s courses.",
    length(creds)
  )
  tryCatch(
    {
      renegades <- purrr::map(creds, function(c) {
        logging::loginfo(
          "... downloading list of renegades in course %s.",
          c$course
        )
        tryCatch(
          {
            if (MUIS::notebook_exists(c, "renegades")) {
              MUIS::read_notebook(c, "renegades")
            } else {
              logging::logwarn(
                "... the block 'renegades' does not exist for course %s.",
                c$course
              )
              no_of_warnings <<- no_of_warnings + 1
              empty
            }
          },
          error = function(e) {
            logging::logerror(e)
            no_of_errors <<- no_of_errors + 1
            empty
          }
        )
      }) |>
        dplyr::bind_rows() |>
        dplyr::mutate(content = stringr::str_trim(content))
      strange <- renegades |>
        dplyr::filter(stringr::str_detect(content, "^[Xx]$", negate = TRUE)) |>
        dplyr::pull(uco)
      if (length(strange) > 0) {
        logging::logwarn(
          "... strange renegade coding for UCOs %s.",
          stringr::str_c(strange, collapse = ", ")
        )
        no_of_warnings <<- no_of_warnings + 1
      }
      renegades |>
        dplyr::filter(stringr::str_detect(content, "^[Xx]$")) |>
        dplyr::pull(uco)
    },
    error = function(e) {
      logging::logerror(e)
      no_of_errors <<- no_of_errors + 1
      integer(0)
    }
  )
}



# writing data ----------------------------------------------------------------

#' Create normalization notebooks.
#'
#' `safely_create_normalized_block()` creates notebooks for normalized points
#' for several courses if they don't exist.
#'
#' @param name (string) full name of the notebook
#' @param shortcut (string) shortcut name of the notebook
#' @param ... credentials
#'
#' @return none, it only creates the blocks in IS
safely_create_normalized_block <- function(name, shortcut, ...) {
  creds <- list(...)
  logging::loginfo(
    "Creating normalization block for %s courses.",
    length(creds)
  )
  purrr::walk(
    creds,
    function(c) {
      if (!MUIS::notebook_exists(c, shortcut)) {
        logging::loginfo("... creating block for %s.", c$course)
        tryCatch(
          MUIS::create_notebook(c, name, shortcut, initialize = FALSE),
          error = function(e) {
            no_of_errors <<- no_of_errors + 1
            logging::logerror(e)
          }
        )
      }
    }
  )
}


#' Writes normalized points to IS.
#'
#' `write_data_to_is()` writes students' normalized points to normalization
#' blocks in IS for several courses.
#'
#' @param students ... a tibble with points
#' @param norm_block ... shortcut name of the block
#' @param ... credentials
#'
#' @return none, it only writes the data to the blocks in IS
write_data_to_is <- function(students, norm_block, ...) {
  creds <- list(...)
  logging::loginfo(
    "Trying to write normalized points for %s courses to IS.",
    length(creds)
  )
  purrr::walk(
    creds,
    function(c) {
      tryCatch(
        {
          logging::loginfo("... writing course %s.", c$course)
          s <- students |>
            dplyr::filter(course == c$course) |>
            dplyr::select(uco = student_uco, value = full_string)
          MUIS::write_notebook(c, norm_block, s)
        },
        error = function(e) {
          logging::logerror(e)
          no_of_errors <<- no_of_errors + 1
        }
      )
    }
  )
}



# point normalization -----------------------------------------------------

# normalize points
normalize_points <- function(
    ucos,
    raw_points,
    renegades,
    a = 20,
    b = 120,
    max_points = 24) {
  valid <- !(ucos %in% renegades)
  mean_points <- mean(raw_points[valid])
  sd_points <- sd(raw_points[valid])
  c1 <- (a + b) / 2
  c2 <- (b - a) / (4 * sd_points)
  norm_points <- (c1 + c2 * (raw_points - mean_points))
  norm_points <- pmin(norm_points, 100)
  norm_points <- pmax(norm_points, 0)
  round(norm_points * max_points / 100)
}
