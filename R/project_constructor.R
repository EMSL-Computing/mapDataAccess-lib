## GENERICS---------------------------------------------------------------------

#' Check object multi-omics data type - specific to MAP
#' @export
.is_datatype <- function(datatype) {
  
  # If Data Type is not a string, return FALSE
  if (is.character(dataype) == FALSE) {
    message("Data Type must be a string.")
    return(FALSE)
  }
  
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



#' Generate a project object to pass data from MAP to pmart
#' 
#' @details Constructs a pmart project object where e_data and f_data are required. 
#'     
#' @param ProjectName Any string to name the project. All spaces and non-alphanumeric
#'    characters will be removed to prevent issues with the visualizations. Required.
#' @param 


