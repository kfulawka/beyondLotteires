# read all excel sheets
read.all.sheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = T))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}