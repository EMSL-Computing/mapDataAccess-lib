#' Test if a file is an edata file
#' 
#' @param edata Must be a dataframe or datatable. Required. 
#' 
#' @return A boolean where TRUE means the file is an acceptable edata file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_edata(pmartRdata::lipid_edata)
#' 
#' }
#' @export
is_edata <- function(edata) {
  
  # If edata is NULL, return FALSE
  if (is.null(edata)) {
    message("No Expression Data provided.")
    return(FALSE)
  }
  
  # edata must have at least 2 samples 
  if (ncol(edata) < 3) {
    message("Expression Data must have at least 3 columns, a 'desciptor' column and at least 2 samples.")
    return(FALSE)
  }
  
  # All columns with the exception of the first column MUST be numeric
  CheckClassCol <- lapply(2:ncol(edata), function(col) {edata[,col] %>% class()}) %>% unlist()
  if (all(CheckClassCol == "numeric") == FALSE) {
    message("All columns in Expression Data must be numeric, with exception of the first 'descriptor' column.")
    return(FALSE)  
  }
  
  # Otherwise, return True
  return(TRUE)
  
}

#' Test if a file is an f_data file. Its companion edata file must be provided. 
#' 
#' @param edata Must be a dataframe or datatable. Required.
#' @param fdata Must be a dataframe or datatable. Required. 
#' 
#' @return A boolean where TRUE means the file is an acceptable f_data file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_fdata(edata = pmartRdata::metab_edata, fdata = pmartRdata::metab_fdata)
#' 
#' }
#' @export
is_fdata <- function(edata, fdata) {
  
  # Make sure neither file is NULL 
  if (is.null(edata)) {
    message("No Expression Data provided.")
    return(FALSE)
  }
  if (is.null(fdata)) {
    message("No Sample Information provided.")
    return(FALSE)
  }
  
  # The number of rows in fdata must be the number of columns in edata minus 1
  if (nrow(fdata) != ncol(edata) - 1) {
    message("The number of rows in Sample Information must match the number of columns in Expression Data minus 1.")
    return(FALSE)
  }
  
  # Make sure the column names of edata[-1] match the column values of a f_data
  ColumnNameCheck <- lapply(1:ncol(fdata), function(col) {
    NamesToCheck <- colnames(edata)[2:ncol(edata)]
    fdata[,col] %in% NamesToCheck %>% all()
  }) %>% unlist() %>% any()
  if (ColumnNameCheck == FALSE) {
    message("One column of Sample Information must match the sample names at the header (top) of the Expression Data edata.")
    return(FALSE)
  }
  
  # Otherwise, return true
  return(TRUE)
  
} 

#' Test if a file is an e_meta file. Its companion edata file must be provided.
#' 
#' @param edata Must be a dataframe or datatable. Required.
#' @param emeta Must be a dataframe or datatable. Required.
#' 
#' @return A boolean where TRUE means the file is an acceptable e_meta file. 
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' is_emeta(edata = pmartRdata::pep_edata, emeta = pmartRdata::pep_emeta)
#' 
#' }
#' @export
is_emeta <- function(edata, emeta) {
  
  # Make sure neither file is NULL 
  if (is.null(edata)) {
    message("No Expression Data provided.")
    return(FALSE)
  }
  if (is.null(emeta)) {
    message("No Biomolecule Information provided.")
    return(FALSE)
  }
  
  # The number of rows must match
  if (nrow(emeta) < nrow(edata)) {
    message("The number of rows in Biomolecule Information must be equal to or greater than the number of rows in Expression Data.")
    return(FALSE)
  }
  
  # Identifiers in edata must exist in emeta 
  IdentifierCheck <- lapply(1:ncol(emeta), function(col) {
    all(edata[,1] %in% emeta[,col])
  }) %>% unlist() %>% any()
  if (IdentifierCheck == FALSE) {
    message("All identifiers in the Expression Data must exist in the Biomolecule Information file.")
    return(FALSE)
  }
  
  # Otherwise, return True
  return(TRUE)
  
}
