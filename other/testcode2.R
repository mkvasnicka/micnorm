library(devtools)

document()
load_all()



micro <- credentials(
  key = "D9eiTfjEf3PEh6V6",
  faculty = 1456,
  course = "BPE_MIE1"
)

mivs <- credentials(
  key = "DeHfGkiVMW0l7oFD",
  faculty = 1456,
  course = "MPE_MIVS"
)

mivsmicro <- tibble::tribble(
  ~course, ~source_seminar, ~target_seminar,
  "MPE_MIVS", "01", "22"
)




res <- normalize_micro(
  micro,
  mivs,
  course_mapping = mivsmicro,
  export_to_IS = FALSE,
  send_mail = FALSE
)

res2 <- normalize_micro(
  micro,
  mivs,
  course_mapping = mivsmicro,
  export_to_IS = FALSE,
  send_mail = FALSE
)

is_same(res, res2)

view_output(res, semin = c("04", "06"))



resmivs <- normalize_micro(
  mivs,
  export_to_IS = FALSE,
  send_mail = FALSE
)
