## GENERICS---------------------------------------------------------------------

#' Check object multi-omics data type - specific to MAP
#' @export
.is_datatype <- function(datatype) {
  
  # Make datatype a string
  datatype <- as.character(datatype)
  
  # Data Type must be of the following classes
  return(datatype %in% c("Peptide-level Label Free","Peptide-level Isobaric",
    "Protein-level Label Free", "Protein-level Isobaric", "Lipidomics-Negative",
    "Lipidomics-Positive", "Metabolomics-GC/LC-MS", "Metabolomics-NMR")
  )
  
}

#' Remove all non-alphanumeric characters
#' @export
.scrub_clean <- function(string) {
  return(gsub("[^[:alnum:]]|[[:space:]]", "", as.character(string)))
}


## PROJECT OBJECT CONSTRUCTORS--------------------------------------------------

#' Generate a simple e_data only project that can only be opened in MODE
#' 
#' @description Construct a project edata object where only edata is required.
#' 
#' @param projectname Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param datatype Must be of the acceptable MAP omic class. See .is_datatype for list. Required. 
#' @param edata Must be a dataframe or datatable. Required. 
#' @param edata_name The path or name for the edata file. Optional. 
#' 
#' @return A project edata object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' project_edata(projectname = "LipidToMode",
#'              datatype = "Lipidomics-Positive",
#'              edata = pmartRdata::lipid_edata,
#'              edata_filename = "lip_edata")
#' 
#' }
#' @export
project_edata <- function(projectname, datatype, edata, edata_filename = NULL) {
  
  # Check data type
  if (.is_datatype(datatype) == FALSE) {
    stop("Data Type is not of the appropriate class.")
  }
  
  # Check edata 
  if (is_edata(edata) == FALSE) {
    stop("edata was not recognized as a proper edata file.")
  }
  
  # Construct project object
  ProjectObject <- list(
    "Project" = list(
      "Name" = .scrub_clean(projectname),
      "DataType" = datatype 
    ),
    "Data" = list(
      "e_data" = edata,
      "e_data_filename" = edata_filename
    )
  )
  
  # Assign the class attribute
  class(ProjectObject) <- "project edata"
  
  # Return
  return(ProjectObject)
  
}

#' Generate a project object to pass data from MAP to pmart
#' 
#' @description Constructs a project pmart object where edata and fdata are required. 
#'     
#' @param projectname Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param datatype Must be of the acceptable MAP omic class. See .is_datatype for list. Required. 
#' @param edata Must be a dataframe or datatable. Required. 
#' @param fdata Must be a dataframe or datatable. Required.
#' @param emeta Must be a dataframe or datatable. Optional. 
#' @param edata_name The path or name for the edata file. Optional. 
#' @param fdata_name The path or name for the fdata file. Optional. 
#' @param emeta_name The path or name for the emeta file. Optional. 
#' 
#' @return A project pmart object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' project_pmart(projectname = "My Peptide Data",
#'              datatype = "Peptide-level Label Free",
#'              edata = pmartRdata::pep_edata,
#'              fdata = pmartRdata::pep_fdata,
#'              emeta = pmartRdata::pep_emeta,
#'              edata_filename = "pep_edata",
#'              fdata_filename = "pep_fdata",
#'              emeta_filename = "pep_emeta")
#' 
#' }
#' @export
project_pmart <- function(projectname, datatype, edata, fdata, emeta = NULL,
                          edata_filename = NULL, fdata_filename = NULL, emeta_filename = NULL) {
  
  # Check data type
  if (.is_datatype(datatype) == FALSE) {
    stop("Data Type is not of the appropriate class.")
  }
  
  # Check edata 
  if (is_edata(edata) == FALSE) {
    stop("edata was not recognized as a proper edata file.")
  }
  
  # Check fdata 
  if (is_fdata(edata, fdata) == FALSE) {
    stop("fdata was not recognized as a proper fdata file.")
  }
  
  # Check emeta (optional)
  if (is.null(emeta) == FALSE) {
    if (is_emeta(edata, emeta) == FALSE) {
      stop("emeta was not recognized as a proper emeta file.")
    }
  }
  
  # Construct project object
  ProjectObject <- list(
    "Project" = list(
      "Name" = .scrub_clean(projectname),
      "DataType" = datatype 
    ),
    "Data" = list(
      "e_data" = edata,
      "e_data_filename" = edata_filename,
      "f_data" = fdata,
      "f_data_filename" = fdata_filename,
      "e_meta" = emeta,
      "e_meta_filename" = emeta_filename
    )
  )
  
  # Assign the class attribute
  class(ProjectObject) <- "project pmart"
  
  # Return
  return(ProjectObject)
  
}

## TODO: project ipmart objects. 


