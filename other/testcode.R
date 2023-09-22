library(devtools)

document()
load_all()

mivs <- credentials("T_EVbceQ0fdFaulj", 1456, "MPE_MIVS")
micro <- credentials("Z6o4VCwTOPPYGWI9", 1456, "BPE_MIE1")

renegades <- get_renegades(micro, mivs)
students <- get_students_attached_to_teachers(micro, mivs)

mivsmicro <- tibble::tribble(
  ~course, ~source_seminar, ~target_seminar,
  "MPE_MIVS", "01", "26"
)
