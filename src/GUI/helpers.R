read_config_to_list <- function(file_path) {
  config <- read.csv(
    file_path,
    comment.char     = "",
    sep              = ",",
    skip             = 0,
    check.names      = FALSE,
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  l <- list()

  for (i in 1 : nrow(config))
    l[config[i, 1]] <- config[i, 2]

  return(l)
}


read_config_to_vector <- function(file_path) {
  config <- read.csv(
    file_path,
    comment.char     = "",
    sep              = ",",
    skip             = 0,
    check.names      = FALSE,
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  v <- config[, 2]
  names(v) <- config[, 1]

  return(v)
}


read_required_fields <- function(file_path) {
  fields <- read.table(
    file_path,
    skip             = 0,
    check.names      = FALSE,
    header           = FALSE,
    stringsAsFactors = FALSE
  )

  return(fields[[1]])
}


parse_params <- function(params, sep = "=") {
  if (params == "")
    return(NULL)

  param_lines <- strsplit(params, "\n")[[1]]
  
  p <- strsplit(param_lines, sep)
  l <- list()

  for (i in seq_along(p))
    l[trimws(p[[i]][1])] <- trimws(p[[i]][2])

  return(l)
}


check_params <- function(params) {
  return(!any(is.na(unlist(params))))
}