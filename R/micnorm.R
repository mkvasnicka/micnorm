no_of_errors <- 0



# mailing -----------------------------------------------------------------

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



# getting data ------------------------------------------------------------

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
                logging::loginfo("... downloadint teachers in course %s.", c$course)
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
                logging::loginfo("... downloadint names of blocks in course %s.", c$course)
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
