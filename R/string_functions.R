camel <- function(x) {
  s <- strsplit(x, "\\.")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = ""
  )
}


remove_underscore <- function(x, replacement = " ") {
  gsub(pattern = "_", replacement = replacement, x = x)
}
