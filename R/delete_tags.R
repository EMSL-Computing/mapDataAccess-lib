#' Remove tags associated with an ID
#' 
#' @details Only valid when using Min.io as the backend storage mechanism.  
#'
#' @param con \code{\link{map_data_connection}} object using Min.io as the backend
#' @param id data object ID, typically returned by \code{\link{put_data}}
#'
#' @return nothing
#' @seealso \code{\link{get_tags}}, \code{\link{set_tags}}
#' @export
delete_tags <- function(con, id) UseMethod("delete_tags")

#' @export
delete_tags.map_dir_connection <- function(con, id) {
  stop("'delete_tags' is not implemented for directory connections")
}

#' @export 
delete_tags.map_minio_connection <- function(con, id) {
  con$client$delete_object_tags(bucket_name=con$bucket,object_name=id)
  invisible()
}