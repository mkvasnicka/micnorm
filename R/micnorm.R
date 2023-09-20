# errors and warnings logging -------------------------------------------------

no_of_errors <- 0
no_of_warnings <- 0



# credentials -----------------------------------------------------------------

#' @export 
#' @importFrom MUIS credentials
MUIS::credentials



# mailing ---------------------------------------------------------------------

#' Send email.
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
    sender,
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


#' Create email from log and send it.
#'
#' @param sender (string) sender's email address
#' @param recipient (string) recipient's email address
#' @param log_file (string) path to log file
#'
#' @return none -- it only sends the mail
create_and_send_mail <- function(sender, recipient, log_file) {
  mail_subject <- ifelse(no_of_errors == 0,
    "Mikro: Seminar points normalization -- everything is o.k.",
    "Mikro: BEWARE: Seminar points normalization failed!"
  )
  send_mail(
    subject = mail_subject,
    body = stringr::str_c(mail_subject, "\n\n\n", readr::read_file(log_file)),
    sender = sender,
    recipient = recipient
  )
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


#' Get all students and attach them to teachers.
#'
#' @param ... credentials
#'
#' @return a tibble with the following columns:
#' - course, seminar,
#' - teacher_uco, teacher_last_name, teacher_first_name,
#' - student_uco, student_last_name, student_first_name,
#' - credentials
get_students_attached_to_teachers <- function(...) {
  students <- get_all_students(...)
  teachers <- get_all_teachers(...) #|>
    # dplyr::select(-credentials)
  dplyr::left_join(
    students,
    teachers,
    by = c("credentials", "course", "seminar")
  ) |>
    dplyr::select(
      course, seminar,
      teacher_uco, teacher_last_name, teacher_first_name,
      student_uco, student_last_name, student_first_name,
      credentials
    )
}



# seminar points --------------------------------------------------------------

#' Get list of all seminar points notebooks.
#' 
#' `get_list_of_existing_notebooks()` downloads names of all existing blocks
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
#' blocks <- get_list_of_existing_notebooks(micprez, mivs)
#' }
get_list_of_existing_notebooks <- function(...) {
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


parse_point_line <- function(
    line,
    excused_call_up_sign = "X",
    max_points = 5) {
    # assume line is character scalar
    stopifnot(is.character(line), length(line) == 1)
    # split line into two parts: call up and raised hand points
    lines <- line |>
        stringr::str_split_fixed("\\|", 2) |>
        _[1, ] |>
        stringr::str_squish()
    # call up points
    call_up <- lines[1]
    if (call_up == "") {
        call_up_points <- 0
        number_of_excused_call_ups <- 0L
        call_up_errors <- 0L
    } else {
        call_up <- call_up |> stringr::str_split_1("\\s+")
        number_of_excused_call_ups <- sum(
            call_up == excused_call_up_sign,
            na.rm = TRUE
        )
        call_up <- call_up |>
            purrr::keep(~ .x != excused_call_up_sign) |>
            purrr::map(~ stringr::str_replace(.x, "(\\d),(\\d)", "\\1.\\2")) |>
            purrr::map_dbl(~ suppressWarnings(as.numeric(.x)))
        call_up_points <- sum(call_up, na.rm = TRUE)
        call_up_errors <- sum(is.na(call_up)) +
            sum(call_up > max_points, na.rm = TRUE)
    }
    # raised hand points
    raised_hand <- lines[2]
    if (raised_hand == "") {
        raised_hand_points <- 0
        raised_hand_errors <- 0L
    } else {
        raised_hand <- raised_hand |>
            stringr::str_replace_all("(\\d),(\\d)", "\\1.\\2") |>
            stringr::str_split_1("\\s+")
        raised_hand <- suppressWarnings(as.numeric(raised_hand))
        raised_hand_points <- sum(raised_hand, na.rm = TRUE)
        raised_hand_errors <- sum(is.na(raised_hand)) +
            sum(raised_hand > max_points, na.rm = TRUE)
    }
    # return
    tibble::tibble(
        input = line,
        call_up_points = call_up_points,
        number_of_excused_call_ups = number_of_excused_call_ups,
        # call_up_errors = call_up_errors,
        raised_hand_points = raised_hand_points,
        # raised_hand_errors = raised_hand_errors,
        errors = call_up_errors + raised_hand_errors
    )
}



#' Read students' points from seminar notebooks.
#' `read_points_from_blocks()` reads all blocks found by
#' get_list_of_existing_notebooks() and returns students' points stored there
#'
#' @param blocks a tibble returned by `get_list_of_existing_notebooks()`
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


#' Get and process students' activity points.
#' 
#' @param ... credentials
#' 
#' @return some tibble
get_activity_points <- function(...) {
  blocks <- get_list_of_existing_notebooks(...)
  activity_points <- read_points_from_blocks(blocks)
  activity_points |>
    dplyr::group_by(uco) |>
    dplyr::arrange(block, .by_group = TRUE) |>
    dplyr::summarize(
      activity_string = stringr::str_c(
        stringr::str_replace_na(points,
          replacement = "-"
        ),
        collapse = " "
      ),
      activity_points = sum(points, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(student_uco = uco)
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



# attendance ------------------------------------------------------------------

#' Get and normalize attendance in several courses.
#'
#' @param ... credentials separated by commas
#'
#' @return a tibble with following columns:
#' - course,
#' - student_uco,
#' - attendance_string,
#' - attendance_points,
#' - normalized_attendance, and
#' - credentials
get_attendance <- function(..., no_of_seminars, max_points_attendance) {
  creds <- list(...)
  purrr::map(creds, MUIS::read_all_presence_points) |>
    dplyr::bind_rows() |>
    dplyr::rename(student_uco = uco) |>
    dplyr::mutate(
      normalized_attendance = attendance_points * max_points_attendance /
        no_of_seminars
    ) |>
    dplyr::select(
      course,
      student_uco,
      attendance_string,
      attendance_points,
      normalized_attendance,
      dplyr::everything()
    )
}



# point augmentation ----------------------------------------------------------

augment_points <- function(students) {
  students |>
    dplyr::mutate(
      activity_points_augmented = activity_points * no_of_seminars /
        (no_of_seminars - excused),
      activity_points_augmented = dplyr::if_else(
        is.nan(activity_points_augmented),
        activity_points,
        activity_points_augmented
      ),
      raw_points = attendance_points + activity_points_augmented
    )
}



# point normalization -----------------------------------------------------

# normalize points
normalize_points_in_one_group <- function(
    ucos,
    raw_points,
    renegades,
    a,
    b,
    max_points) {
  valid <- !(ucos %in% renegades)
  mean_points <- mean(raw_points[valid])
  sd_points <- stats::sd(raw_points[valid])
  c1 <- (a + b) / 2
  c2 <- (b - a) / (4 * sd_points)
  norm_points <- (c1 + c2 * (raw_points - mean_points))
  norm_points <- pmin(norm_points, 100)
  norm_points <- pmax(norm_points, 0)
  round(norm_points * max_points / 100)
}


normalize_points <- function(
    students,
    max_points_activity,
    activity_const_a,
    activity_const_b) {
  students |>
    dplyr::group_by(teacher_uco) |>
    dplyr::mutate(
      norm_points = normalize_points_in_one_group(
        student_uco,
        raw_points,
        renegades,
        a = activity_const_a,
        b = activity_const_b,
        max_points = max_points_activity
      )
    ) |>
    dplyr::ungroup()
}



# output string ---------------------------------------------------------------

add_output_string <- function(students) {
  dplyr::mutate(full_string = stringr::str_c(
    "Body za účast: ", attendance_points,
    " (omluveno: ", excused, ")\n",
    "(účast: ", attendance_string, ")\n\n",
    "Body za aktivitu: ", round(activity_points, 1), "\n",
    "(body: ", activity_string, ")\n\n",
    "Hrubé body za účast a aktivitu: ", round(raw_points, 1), "\n",
    "Normované body za účast a aktivitu: *", norm_points
  ))
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
#' @param norm_name ... (string) full name of the block
#' @param norm_block ... (string) shortcut name of the block
#' @param ... credentials separated by commas
#'
#' @return none, it only writes the data to the blocks in IS
#'
#' @details if the notebooks don't exist, it creates them
write_data_to_is <- function(students, norm_name, norm_block, ...) {
  creds <- list(...)
  logging::loginfo(
    "Trying to write normalized points for %s courses to IS.",
    length(creds)
  )
  safely_create_normalized_block(norm_name, norm_block, ...)
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



# main function ---------------------------------------------------------------

#' Read IS notebooks, normalize students' points, and write it back to IS.
#'
#' @param `...` credentials separated by commas
#' @param no_of_seminars (optional, integer) number of seminars in the given
#'   year; implicit value is 12L
#' @param max_points_attendance (optional, number) maximum normalized points
#'   for attendance; implicit value is 6
#' @param max_points_activity (optional, number) maximum normalized points
#'   for activity; implicit value is 24
#' @param activity_const_a (optional, number) normalization constant A;
#'   implicit value is 20
#' @param activity_const_b (optional, number) normalization constant B;
#'   implicit value is 120
#' @param norm_name (optional, string) name of notebook with normalized points
#' @param norm_block (optional, string) shortcut of notebook with normalized
#'   points
#' @param log_folder (optional, string) path to folder where logs are written;
#'  if it doesn't exist, it is created
#' @param group_file_name (optional, string) name of file, where partial data
#'  are written
#'
#' @return (silently) tibble with data created
#' 
#' @export
#
normalize_micro <- function(
    ...,
    no_of_seminars = 12L,
    max_points_attendance = 6,
    max_points_activity = 24,
    activity_const_a = 20,
    activity_const_b = 120,
    norm_name = "Normované body za průběžnou práci na semináři",
    norm_block = "bodsemin",
    log_folder = "logs",
    group_file_name = "last_groupings.RData",
    sender  = "847@muni.cz",
    recipient = sender) {
  # create log folder if necessary and start logging
  if (!dir.exists(log_folder)) {
    dir.create(log_folder)
  }
  log_file <- file.path(
    log_folder,
    stringr::str_c("micro_normalization_", Sys.Date(), ".log")
  )
  logging::addHandler(logging::writeToFile, file = log_file)
  no_of_errors <<- 0
  no_of_warnings <<- 0
  # log the start
  logging::loginfo(
    "Starting Micro point normalization on %s.",
    Sys.info()["nodename"]
  )
  logging::loginfo(
    "I would write the normalize points to block with shortcut %s.",
    norm_block
  )
  try({
    # get renegades (i.e. students that stopped working within the term)
    renegades <- get_renegades(...)
    # get the data on students and teachers, join them, and save
    # saving needed because after the end of the term some students may drop
    # and the later computation would be off
    students <- get_students_attached_to_teachers(...)
    save(students, file = group_file_name)
    # add activity points
    students <- students |>
      dplyr::left_join(
        get_activity_points(...),
        by = c("credentials", "student_uco")
      ) |>
      # augment them for illness and various number of examinations
      augment_points() |>
      # normalize them
      normalize_points(
        max_points_activity,
        activity_const_a,
        activity_const_b
      ) |>
      # add and normalize attendance points
      dplyr::left_join(
        get_attendance(
          ...,
          no_of_seminars = no_of_seminars,
          max_points_attendance = max_points_attendance
        ),
        by = c("credentials", "student_uco")
      ) |>
      # add output string
      add_output_string() |>
      # create blocks for normalization and write the normalized points to IS
      write_data_to_is(norm_name, norm_block, ...)
  })
  # log the end
  logging::loginfo("Stopping Micro point normalization.")
  logging::loginfo(
    "Finished with %s errors and %s warnings",
    no_of_errors, no_of_warnings
  )
  # send mail
  create_and_send_mail(sender, recipient, log_file)
  # return invisibly
  invisible(students)
}
