# internal function

get_unique_id <- function(con, ...) UseMethod("get_unique_id")


get_unique_id.map_dir_connection <- function(con, ...) {
  id <- uuid::UUIDgenerate()
  # make sure it's unique
  while (file.exists(file.path(con$dir, id))) {
    id <- uuid::UUIDgenerate()
  }
  return(id)
}

get_unique_id.map_minio_connection <- function(con, ...) {
  id <- uuid::UUIDgenerate()
  # make sure it's unique
  while (id %in% reticulate::iterate(con$client$list_objects(con$bucket), function(x) x$object_name)) {
    id <- uuid::UUIDgenerate()
  }
  return(id)
}