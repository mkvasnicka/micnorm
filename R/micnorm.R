# errors and warnings logging -------------------------------------------------

the <- new.env(parent = emptyenv())
the$no_of_errors <- 0
the$no_of_warnings <- 0



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
  mail_subject <- ifelse(the$no_of_errors == 0,
    "Mikro: Seminar points normalization -- everything is o.k.",
    "Mikro: BEWARE: Seminar points normalization failed!"
  )
  send_mail(
    subject = mail_subject,
    body = stringr::str_c(
      mail_subject,
      if (the$no_of_errors > 0)
        "\n\nData are NOT written to IS!",
      "\n\n\n",
      "Number of errors: ", the$no_of_errors, "\n",
      "Number of warnings: ", the$no_of_warnings, "\n",
      "\n\n\n",
      readr::read_file(log_file)
    ),
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
            the$no_of_errors <- the$no_of_errors + 1
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
      the$no_of_errors <- the$no_of_errors + 1
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
            the$no_of_errors <- the$no_of_errors + 1
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
      the$no_of_errors <- the$no_of_errors + 1
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


#' If there are more courses, join their seminars.
#'
#' `unite_courses()` joins seminars of several courses if they are the same
#' (e.g. BPE_MIE1 and MPE_MIVS)
#'
#' @param students (tibble) students
#' @param mapping (tibble) mapping of courses; must include columns:
#'  - course,
#' - source_seminar, and
#' - target_seminar
#' 
#' @return a tibble with the same columns as `students`
#' 
#' @details if `mapping` is `NULL`, it returns `students` unchanged
unite_courses <- function(students, mapping) {
  if (is.null(mapping)) {
    return(students)
  }
  dplyr::left_join(
    students,
    mapping,
    by = c("course", "seminar" = "source_seminar")
  ) |>
    dplyr::mutate(
      seminar = dplyr::if_else(is.na(target_seminar), seminar, target_seminar)
    ) |> 
    dplyr::select(-target_seminar)
}



# activity points -------------------------------------------------------------

#' Get list of all activity points notebooks.
#'
#' `get_list_of_existing_notebooks()` downloads names of all existing blocks
#' within several courses
#'
#' @param `...` credentials; separated by commas
#' @param name_mask (string) regular expression that says how are named
#'   the notebooks that include activity points
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
get_list_of_existing_notebooks <- function(
    ...,
    name_mask = "^bodysemin\\d{2}$") {
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
            the$no_of_errors <- the$no_of_errors + 1
            NULL
          }
        )
      }) |>
        dplyr::bind_rows() |>
        dplyr::filter(stringr::str_detect(shortcut, name_mask)) |>
        dplyr::arrange(shortcut, course)
    },
    error = function(e) {
      logging::logerror(e)
      the$no_of_errors <- the$no_of_errors + 1
      NULL
    }
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
      the$no_of_errors <- the$no_of_errors + 1
      NULL
    }
  )
}


# see tests
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
    number_of_non_excused_call_ups <- 0L
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
    number_of_non_excused_call_ups <- sum(!is.na(call_up))
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
    number_of_non_excused_call_ups = number_of_non_excused_call_ups,
    number_of_excused_call_ups = number_of_excused_call_ups,
    # call_up_errors = call_up_errors,
    raised_hand_points = raised_hand_points,
    # raised_hand_errors = raised_hand_errors,
    errors = call_up_errors + raised_hand_errors
  )
}


# prettyfies points to format - | -
prettyfy_points <- function(s) {
  s1 <- stringr::str_split_i(s, "\\|", 1) |> 
    stringr::str_squish()
  s2 <- stringr::str_split_i(s, "\\|", 2) |> 
    stringr::str_squish()
  # s <- stringr::str_split_1(s, "\\|") |> 
  #   stringr::str_squish()
  stringr::str_c(
    ifelse(s1 == "", "-", s1),
    " | ",
    ifelse(is.na(s2) | s2 == "", "-", s2)
  )
}


#' Get and process students' activity points.
#'
#' @param ... credentials
#' @param name_mask (string) regular expression that says how are named 
#'   the notebooks where activity points are stored
#'
#' @return tibble with following columns:
#' - credentials
#' - student_uco
#' -call_up_points
#' - number_of_non_excused_call_ups
#' - number_of_excused_call_ups
#' - raised_hand_points
#' - activity_point_string
get_activity_points <- function(..., name_mask = "^bodysemin\\d{2}$") {
  blocks <- get_list_of_existing_notebooks(..., name_mask = name_mask)
  activity_points <- read_points_from_blocks(blocks)
  points <- activity_points$content |>
    stringr::str_replace_na("") |>
    purrr::map(parse_point_line) |>
    dplyr::bind_rows()
  activity_points <- dplyr::bind_cols(activity_points, points)
  # log bugs in notebooks
  misshaped <- activity_points |>
    dplyr::filter(errors > 0)
  for (k in seq_len(nrow(misshaped))) {
    logging::logwarn(
      stringr::str_c(
        "... activity points are crippled in course %s,",
        " notebook %s, uco %s: content '%s'"
      ),
      misshaped$course[k],
      misshaped$notebook[k],
      misshaped$uco[k],
      misshaped$content[k]
    )
  }
  the$no_of_warnings <- the$no_of_warnings + nrow(misshaped)
  #
  valid_notebooks <- activity_points |>
    dplyr::summarize(valid = sum(input != "") > 0, .by = notebook) |>
    dplyr::filter(valid) |>
    dplyr::pull(notebook)
  point_string <- activity_points |>
    dplyr::filter(notebook %in% valid_notebooks) |>
    dplyr::group_by(credentials, uco) |>
    dplyr::arrange(credentials, uco, notebook, .by_group = TRUE) |>
    dplyr::summarize(
      activity_point_string = stringr::str_c(
        "(", stringr::str_extract(notebook, "\\d{2}"), ") ",
        prettyfy_points(input),
        collapse = "  "
      ),
      .groups = "drop"
    )
  # aggregation
  activity_points <- activity_points |>
    dplyr::group_by(credentials, uco) |>
    dplyr::summarize(
      call_up_points = sum(call_up_points, na.rm = TRUE),
      number_of_non_excused_call_ups = sum(
        number_of_non_excused_call_ups,
        na.rm = TRUE
      ),
      number_of_excused_call_ups = sum(
        number_of_excused_call_ups,
        na.rm = TRUE
      ),
      raised_hand_points = sum(raised_hand_points, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  #
  dplyr::left_join(
    activity_points,
    point_string,
    by = c("credentials", "uco")
  ) |>
    dplyr::rename(student_uco = "uco") |>
    dplyr::mutate(no_of_valid_seminars = length(valid_notebooks))
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
              the$no_of_warnings <<- the$no_of_warnings + 1
              empty
            }
          },
          error = function(e) {
            logging::logerror(e)
            the$no_of_errors <- the$no_of_errors + 1
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
        the$no_of_warnings <- the$no_of_warnings + 1
      }
      renegades |>
        dplyr::filter(stringr::str_detect(content, "^[Xx]$")) |>
        dplyr::pull(uco)
    },
    error = function(e) {
      logging::logerror(e)
      the$no_of_errors <- the$no_of_errors + 1
      integer(0)
    }
  )
}



# attendance ------------------------------------------------------------------

#' Get alternative attendance points from IS.
#'
#' `get_alternative_attendance()` downloads alternative attendance points
#' from IS.
#'
#' @param ... credentials
#' @param alt_attendance_notebook (string) name of the notebook where
#'  alternative attendance points are stored
#'
#' @return a tibble with following columns:
#' - course,
#' - student_uco,
#' - alt_attendance_points
get_alternative_attendance <- function(..., alt_attendance_notebook) {
  empty_tab <- tibble::tibble(
    course = character(0),
    uco = integer(0),
    content = character(0)
  )
  f <- function(c, alt_attendance_notebook) {
    if (MUIS::notebook_exists(c, alt_attendance_notebook)) {
      logging::loginfo(
        "... downloading alternative attendance in course %s.",
        c$course
      )
      tryCatch(
        MUIS::read_notebook(c, alt_attendance_notebook),
        error = function(e) {
          logging::logerror(e)
          the$no_of_errors <- the$no_of_errors + 1
          NULL
        }
      )
    } else {
      logging::logwarn(
        "... the block '%s' does not exist for course %s.",
        alt_attendance_notebook, c$course
      )
      the$no_of_warnings <- the$no_of_warnings + 1
      NULL
    }
  }
  creds <- list(...)
  alt_attendance <- purrr::map(creds, f, alt_attendance_notebook) |>
    dplyr::bind_rows(empty_tab) |>
    dplyr::mutate(
      content = stringr::str_replace_na(content, ""),
      content = stringr::str_squish(content),
      alt_attendance_points = dplyr::if_else(
        content == "",
        0L,
        suppressWarnings(as.integer(content))
      )
    )
  #
  misshaped <- alt_attendance |>
    dplyr::filter(is.na(alt_attendance_points))
  for (r in seq_len(nrow(misshaped))) {
    logging::logwarn(
      stringr::str_c(
        "... alternative attendance points are crippled in course %s,",
        " uco %s: content '%s'"
      ),
      misshaped$course[r],
      misshaped$uco[r],
      misshaped$content[r]
    )
  }
  the$no_of_warnings <- the$no_of_warnings + nrow(misshaped)
  #
  alt_attendance |>
    dplyr::mutate(
      alt_attendance_points = dplyr::if_else(
        is.na(alt_attendance_points),
        0L,
        alt_attendance_points
      )
    ) |>
    dplyr::select(
      course,
      student_uco = uco,
      alt_attendance_points
    )
}

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
get_attendance <- function(
    ...,
    no_of_seminars,
    max_points_attendance,
    alt_attendance_notebook) {
  creds <- list(...)
  alt <- get_alternative_attendance(
    ...,
    alt_attendance_notebook = alt_attendance_notebook
  )
  purrr::map(creds, MUIS::read_all_presence_points) |>
    dplyr::bind_rows() |>
    dplyr::rename(student_uco = uco) |>
    dplyr::left_join(alt, by = c("course", "student_uco")) |>
    dplyr::select(
      course,
      student_uco,
      attendance_string,
      attendance_points,
      alt_attendance_points,
      dplyr::everything()
    )
}


#' Add attendance data to a table
#'
#' This function adds attendance data to a table.
#'
#' @param tab A tibble containing the input table.
#' @param ... credentials separated by commas.
#' @param no_of_seminars The total number of seminars.
#' @param max_points_attendance The maximum points that can be obtained
#'  for attendance.
#' @param alt_attendance_notebook The name of the notebook with an alternative
#'   attendance.
#'
#' @return A tibble containing the input table with attendance data added.
add_attendance <- function(
    tab,
    ...,
    no_of_seminars,
    max_points_attendance,
    alt_attendance_notebook) {
  no_of_valid_seminars <- tab$no_of_valid_seminars[1]
  dplyr::left_join(
    tab,
    get_attendance(
      ...,
      no_of_seminars = no_of_valid_seminars, # no_of_seminars,
      max_points_attendance = max_points_attendance,
      alt_attendance_notebook = alt_attendance_notebook
    ) |>
      dplyr::select(-course),
    by = c("credentials", "student_uco")
  ) |>
    dplyr::mutate(
      attendance_points = dplyr::if_else(
        is.na(attendance_points),
        0L,
        attendance_points
      ),
      alt_attendance_points = dplyr::if_else(
        is.na(
          alt_attendance_points
        ),
        0L,
        alt_attendance_points
      ),
      attendance_string = dplyr::if_else(
        is.na(attendance_string),
        "",
        attendance_string
      ),
      attendance_points = attendance_points + alt_attendance_points,
      normalized_attendance = attendance_points * max_points_attendance /
        no_of_valid_seminars # no_of_seminars
    )
}



# point augmentation ----------------------------------------------------------

#' Augment activity points for different number of call ups and for illness.
#' 
#' `augment_points()` augments activity points for different number of call ups.
#'
#' @param students (tibble) students
#'
#' @return a tibble with same columns as students + raw_points
augment_points <- function(students) {
  students |>
    dplyr::group_by(teacher_uco) |>
    dplyr::mutate(
      max_calls = max(
        number_of_non_excused_call_ups + number_of_excused_call_ups,
        na.rm = TRUE
      ),
      raw_points = call_up_points / number_of_non_excused_call_ups * max_calls,
      raw_points = dplyr::if_else(is.finite(raw_points), raw_points, 0),
      raw_points = raw_points + raised_hand_points
    ) |> 
    dplyr::ungroup()
}



# point normalization -----------------------------------------------------

# normalize points in one group
normalize_points_in_one_group <- function(
    ucos,
    raw_points,
    renegades,
    a,
    b,
    max_points) {
  valid <- !(ucos %in% renegades)
  mean_points <- mean(raw_points[valid], na.rm = TRUE)
  sd_points <- stats::sd(raw_points[valid], na.rm = TRUE)
  c1 <- (a + b) / 2
  c2 <- (b - a) / (4 * sd_points)
  norm_points <- (c1 + c2 * (raw_points - mean_points))
  norm_points <- pmin(norm_points, 100)
  norm_points <- pmax(norm_points, 0)
  norm_points * max_points / 100
}

#' Normalize points for activity on seminar.
#'
#' @param students (tibble)
#' @param max_points_activity (number) maximum normalized points
#'   for activity
#' @param activity_const_a (number) normalization constant A
#' @param activity_const_b (number) normalization constant B
#'
#' @return a tibble
normalize_points <- function(
    students,
    renegades,
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



# output strings --------------------------------------------------------------

#' Format number.
#'
#' `format_number()` formats a number for output string---it replaces NA and
#' Inf with 0 and rounds the number to 3 decimal places.
#'
#' @param x (number) number to be formatted
#'
#' @return a number
format_number <- function(x) {
  dplyr::if_else(is.na(x) | is.infinite(x), 0, x) |>
    round(3)
}


#' Add string describing students' activity and attendance.
#'
#' `add_output_string()` adds a string describing students' activity 
#' and attendance to students' tibble.
#'
#' @param students (tibble) students
#'
#' @return a tibble with same columns as students + output_string
add_output_string <- function(
  students,
  max_points_attendance,
  max_points_activity) {
  students |>
    dplyr::mutate(
      output_string = stringr::str_c(
        # total points
        "Počet normovaných bodů za účast a práci na semináři: *",
        round(norm_points + normalized_attendance),
        " z ", (max_points_activity + max_points_attendance)," možných.\n",
        "\n",
        "Tyto normované body se skládají ze dvou částí:\n",
        "\n",
        #
        # activity
        "Počet normovaných bodů za aktivitu: ", format_number(norm_points), 
        " z ", max_points_activity, " možných.\n",
        "(Normované body za aktivitu mohou růst i klesat.)\n",
        "Počet hrubých bodů za vyvolání: ",
        format_number(call_up_points), ".\n",
        "Počet vyvolání: ",
        number_of_non_excused_call_ups + number_of_excused_call_ups, ", ",
        "z toho omluveno: ", number_of_excused_call_ups, ".\n",
        "Maximální počet vyvolání ve skupině: ", max_calls, ".\n",
        "(Body se vyvolání se násobí koeficientem ", 
        max_calls, " / ", number_of_non_excused_call_ups, " = ",
        format_number(max_calls / number_of_non_excused_call_ups),
        ".)\n",
        "Hrubé body za přihlášení se: ",
        format_number(raised_hand_points),
        ".\n",
        "Přepočtené body za aktivitu: ", format_number(raw_points), ".\n",
        "Body: ", activity_point_string, ".\n",
        "\n",
        #
        # attendance
        "Počet normovaných bodů za účast: ",
        format_number(normalized_attendance),
        " z ", max_points_attendance, " možných.\n",
        "(Normované body za účast mohou růst i klesat.)\n",
        "Počet účastí: ", attendance_points, " z ",
        no_of_valid_seminars, " proběhlých seminářů.\n",
        "Účast na semináři: ", attendance_string, "\n",
        "Počet účastí na náhradním termínu: ", alt_attendance_points, ".\n"
      )
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
            the$no_of_errors <- the$no_of_errors + 1
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
            dplyr::select(uco = student_uco, value = output_string)
          MUIS::write_notebook(c, norm_block, s)
        },
        error = function(e) {
          logging::logerror(e)
          the$no_of_errors <- the$no_of_errors + 1
        }
      )
    }
  )
}



# logging ---------------------------------------------------------------------

#' Start logging.
#'
#' `start_logging()` creates a log folder if necessary and starts logging into
#' a file. It also logs the start of the script.
#'
#' @param log_folder (string) path to folder where logs are written
#' @param norm_block (string) shortcut of notebook with normalized points
#'
#' @return path to log_file
start_logging <- function(log_folder, norm_block) {
  # create log folder if necessary and start logging
  if (!dir.exists(log_folder)) {
    dir.create(log_folder)
  }
  # start logging into a file
  current_time <- Sys.time() |>
    as.character() |>
    stringr::str_remove("\\a+$") |>
    stringr::str_remove("\\.\\d+\\s*$") |>
    stringr::str_replace_all(":", "-") |>
    stringr::str_replace_all("\\s+", "_")
  log_file <- file.path(
    log_folder,
    stringr::str_c("micro_normalization_", current_time, ".log")
  )
  logging::addHandler(logging::writeToFile, file = log_file)
  the$no_of_errors <- 0
  the$no_of_warnings <- 0
  # log the start
  logging::loginfo(
    "Starting Micro point normalization on %s.",
    Sys.info()["nodename"]
  )
  logging::loginfo(
    "I would write the normalize points to block with shortcut %s.",
    norm_block
  )
  # return path to log_file
  log_file
}



# main function ---------------------------------------------------------------

#' Read IS notebooks, normalize students' points, and write it back to IS.
#'
#' @param `...` credentials separated by commas
#' @param course_mapping (optional, tibble) mapping of courses; must include
#'   columns: course, source_seminar, and target_seminar
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
#' @param activity_name_mask (optional, string) regular expression that says
#'   how are named the notebooks where activity points are stored;
#'   implicitly "^bodysemin\\d{2}$",
#' @param norm_name (optional, string) name of notebook with normalized points
#' @param norm_block (optional, string) shortcut of notebook with normalized
#'   points
#' @param log_folder (optional, string) path to folder where logs are written;
#'  if it doesn't exist, it is created
#' @param group_file_name (optional, string) name of file, where partial data
#'  are written
#' @param sender (optional, string) email address of sender
#' @param recipient (optional, string) email address of recipient
#' @param export_to_IS (optional, logical) whether to export the data to IS;
#'  implicit value is TRUE
#'
#' @return (silently) tibble with data created
#'
#' @export
#
normalize_micro <- function(
    ...,
    course_mapping = NULL,
    no_of_seminars = 12L,
    max_points_attendance = 6,
    max_points_activity = 24,
    activity_const_a = 20,
    activity_const_b = 120,
    activity_name_mask = "^bodysemin\\d{2}$",
    alt_attendance_notebook = "preznahr",
    norm_name = "Normované body za účast a průběžnou práci na semináři",
    norm_block = "bodsemin",
    log_folder = "logs",
    group_file_name = "last_groupings.RData",
    sender = "847@muni.cz",
    recipient = sender,
    export_to_IS = TRUE,
    send_mail = TRUE) {
  log_file <- start_logging(log_folder, norm_block)
  try({
    # get renegades (i.e. students that stopped working within the term)
    renegades <- get_renegades(...)
    # get the data on students and teachers, join them, and save
    # saving needed because after the end of the term some students may drop
    # and the later computation would be off
    students <- get_students_attached_to_teachers(...) |>
      unite_courses(course_mapping)
    save(students, file = group_file_name)
    # add activity points
    students <- students |>
      dplyr::left_join(
        get_activity_points(..., name_mask = activity_name_mask),
        by = c("credentials", "student_uco")
      ) |>
      # augment them for illness and various number of examinations
      augment_points() |>
      # normalize them
      normalize_points(
        renegades,
        max_points_activity,
        activity_const_a,
        activity_const_b
      ) |>
      # add and normalize attendance points
      add_attendance(
        ...,
        no_of_seminars = no_of_seminars,
        max_points_attendance = max_points_attendance,
        alt_attendance_notebook = alt_attendance_notebook
      ) |>
      # add output string
      add_output_string(
        max_points_attendance = max_points_attendance,
        max_points_activity = max_points_activity
      )
    # create blocks for normalization and write the normalized points to IS
    if (export_to_IS && the$no_of_errors == 0) {
      write_data_to_is(students, norm_name, norm_block, ...)
    }
  })
  # log the end
  logging::loginfo("Stopping Micro point normalization.")
  logging::loginfo(
    "Finished with %s errors and %s warnings",
    the$no_of_errors, the$no_of_warnings
  )
  # send mail
  if (send_mail) {
    create_and_send_mail(sender, recipient, log_file)
  }
  # return invisibly
  invisible(students)
}
