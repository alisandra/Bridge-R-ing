library(readxl)
library(gdata)

read_sheet <- function(filename, sheet, comment.char='#', strip_quotes=TRUE, strip_na=TRUE, ...){
  if (! strip_quotes){
    out <- read.xls(filename=filename, sheet=sheet, comment.char=comment.char, ...)
  }else{
    out <- tryCatch({
      f <- xls2tab(filename, sheet=sheet)
      raw <- readLines(f)
      raw <- gsub("\\\"", "", raw)
      read.delim(text=raw, comment.char=comment.char, ...)
    }, warning = function(w) {
    }, error = function(e) {
      data.frame()
    }, finally = {
      summary_f <- summary(f)
      close(f)
      file.remove(summary_f$description)
    })
  }
  if (strip_na){
    # remove columns with only NAs
    to_keep <- which(apply(out, 2, function(x) ! all(is.na(x))))
    out <- out[, to_keep]

  }
  # todo, cleanup, for instance of columns that are entirely NA (and unnamed) 
  return(out)
}

read_all_sheets <- function(filename, comment.char='#', strip_quotes=TRUE, strip_na=TRUE, ...){
  sheet_names <- excel_sheets(filename)
  sheet_list <- lapply(sheet_names, function(s) read_sheet(filename,s, comment.char=comment.char,
                                                           strip_quotes=strip_quotes, strip_na=strip_na,  ...))
  names(sheet_list) <- sheet_names 
  return(sheet_list)
}
