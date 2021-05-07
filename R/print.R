#' @export
print.map_data_connection <- function(x, ...) {
  cat(paste0("type: ", class(x)[1], "\n"))
  for (k in names(x)) {
    cat(paste0(k, ": ", as.character(x[[k]]), "\n"))
  }
  invisible()
}

#' @export
toString.map_data_connection <- function(x, ...) {
  s <- paste0("{type=", class(x)[1], "; ")
  s <- paste0(s, paste0(names(x), "=", as.character(unlist(x)), collapse="; "), "}")
  return(s)
}