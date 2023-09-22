library(devtools)

document()
load_all()

activity_name_mask <- "goo\\d{2}" # "^bodysemin\\d{2}$"
alt_attendance_notebook <- "goonahr"

mivs <- credentials("T_EVbceQ0fdFaulj", 1456, "MPE_MIVS")
micro <- credentials("Z6o4VCwTOPPYGWI9", 1456, "BPE_MIE1")

mivsmicro <- tibble::tribble(
  ~course, ~source_seminar, ~target_seminar,
  "MPE_MIVS", "01", "26"
)

renegades <- get_renegades(micro, mivs)
students <- get_students_attached_to_teachers(micro, mivs) |>
  unite_courses(mivsmicro)

students <- students |>
  dplyr::left_join(
    get_activity_points(micro, mivs, name_mask = activity_name_mask),
    by = c("credentials", "student_uco")
  )

students <- students |>
  augment_points()

students <- students |>
  normalize_points(24, 20, 120)

attendance <- get_attendance(
  micro, mivs,
  no_of_seminars = 12,
  max_points_attendance = 6,
  alt_attendance_notebook = alt_attendance_notebook
)

students <- dplyr::left_join(
  students,
  attendance,
  by = c("credentials", "student_uco")
)

students <- students |>
  add_output_string()


# ----------------------------


blocks <- get_list_of_existing_notebooks(mivs, name_mask = activity_name_mask)
activity_points <- read_points_from_blocks(blocks)
points <- activity_points$content |>
  stringr::str_replace_na("") |>
  purrr::map(parse_point_line) |>
  dplyr::bind_rows()
activity_points <- dplyr::bind_cols(activity_points, points)


activity_points |>
  dplyr::group_by(credentials, uco) |>
  dplyr::arrange(credentials, uco, notebook, .by_group = TRUE) |>
  dplyr::summarize(
    point_string = stringr::str_c(
      "(", stringr::str_extract(notebook, "\\d{2}"), ") ",
      input,
      collapse = " || "),
      .groups = "drop"
  ) |> 
  dplyr::mutate(point_string = stringr::str_squish(point_string))


get_alternative_attendance(mivs, alt_attendance_notebook = "goonahr")

get_attendance(
  mivs,
  no_of_seminars = 12,
  max_points_attendance = 6,
  alt_attendance_notebook = "goonahr"
)



# ----------------------------


students <- normalize_micro(mivs,
  activity_name_mask = "goo\\d{2}",
  alt_attendance_notebook = "goonahr",
  export_to_IS = FALSE
)
