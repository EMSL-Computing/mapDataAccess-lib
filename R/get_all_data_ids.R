#' Get all existing object IDs
#' 
#' @details If the SHINYPROXY_USERNAME environment variable is set this will 
#' return all IDs for that user, otherwise it will return all IDs in the 
#' data connection.
#' 
#' @param con \code{\link{map_data_connection}} object 
#'
#' @return a character vector of IDs of existing data
#' @export
get_all_data_ids <- function(con, ...) UseMethod("get_all_data_ids")

#' @export
get_all_data_ids.map_dir_connection <- function(con, ...) {
  if (con$verbose) message("Getting all data IDs")
  curr_ids <- list.files(con$dirname, recursive=TRUE)
  return(curr_ids)
}

#' @export
get_all_data_ids.map_minio_connection <- function(con, ...) {
  if (con$verbose) message("Getting all data IDs")
  curr_ids = reticulate::iterate(
    con$client$list_objects(con$bucket, prefix=con$directory, recursive=TRUE), 
    function(x) x$object_name)
  if (!is.null(con$directory)) {
    curr_ids <- gsub(paste0(con$directory, "/"), "", curr_ids)
  }
  return(curr_ids)
}