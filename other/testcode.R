library(devtools)

document()
load_all()

activity_name_mask <- "goo\\d{2}"  # "^bodysemin\\d{2}$"

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
