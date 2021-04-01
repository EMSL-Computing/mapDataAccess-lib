#' @export
print.map_data_connection <- function(obj) {
  cat(paste0("type: ", class(obj)[1], "\n"))
  for (k in names(obj)) {
    cat(paste0(k, ": ", obj[[k]], "\n"))
  }
  invisible()
}

#' @export
toString.map_data_connection <- function(x, ...) {
  s <- paste0("{type=", class(x)[1], "; ")
  s <- paste0(s, paste0(names(x), "=", unlist(x), collapse="; "), "}")
  return(s)
}