#' Remove data object from a map_data_connection object using a UUID 
#'
#' @param con \code{\link{map_data_connection}} object
#' @param id ID returned by \code{\link{put_data} or \code{\link{put_file}}
#' @param ... 
#'
#' @return TRUE or FALSE indicating success
#' @export
remove_data <- function(con, id, ...) UseMethod("remove_data")

#' @export
remove_data.map_dir_connection <- function(con, id, ...) {
  if (file.exists(file.path(con$dir, id))) {
    file.remove(file.path(con$dir, id))
  }
  return(TRUE)
}

#' @export
remove_data.map_minio_connection <- function(con, id, ...) {
  con$client$remove_object(con$bucket, id)
  return(TRUE)
}