#' Retrieve file object from a moap_data_connection object using a UUID 
#'
#' @param con \code{\link{map_data_connection}} object
#' @param id file object ID, typically returned by \code{\link{put_file}}
#'
#' @return path to local file copy (generated with \code{\link{tempfile}} function)
#' @seealso \code{\link{put_file}}, \code{\link{map_data_connection}}
#' @export
get_file <- function(con, id, filename, ...) UseMethod("get_file")

#' @export
get_file.map_dir_connection <- function(con, id, ...) {
  if (con$verbose) message(sprintf("Retrieving %s/%s", con$bucket, id))
  fname <- file.path(con$dir, id)
  if (!file.exists(fname)) 
    stop(sprintf("Invalid id: %s", id))
  dest_path <- tempfile()
  file.copy(from = fname, to = dest_path)
  
  return(dest_path)
}

#' @export
get_file.map_minio_connection <- function(con, id, filename, ...) {
  if (!is.null(con$directory)) {
    id <- paste(con$directory, id, sep="/")
  }
  
  if (con$verbose) message(sprintf("Retrieving %s/%s", con$bucket, id))
  
  dest_path <- tempfile()
  con$client$fget_object(bucket_name=con$bucket, object_name=id, file_path=dest_path)
  return(dest_path)
}