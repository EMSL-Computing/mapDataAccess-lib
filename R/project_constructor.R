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

#' Generate a project object to pass data from MAP to ipmart
#' 
#' @description Constructs a project ipmart object from pmart projects and midpoints.
#' 
#' @param objects List of pmart projects or midpoints at the same tab (normalization or statistics).
#'    Must contain 2-5 objects. There can be
#'    no more than 2 metabolomics (1 of: NMR or GC/LC-MS), 
#'    no more than 2 lipidomics datasets, 
#'    and no more than 1 proteomics (peptide or protein) dataset. Required. 
#'
#' @return A project ipmart object
#' @examples 
#' \dontrun{
#' 
#' library(pmartRdata)
#' 
#' # Generate midpoint with the example in midpoint_pmart and save result as "midpoint"
#' 
#' # Make a metabolomics GC/LC MS project
#' metab_project <- project_pmart(projectname = "My Metab Data",
#'                                datatype = "Metabolomics-GC/LC-MS",
#'                                edata = pmartRdata::metab_edata,
#'                                fdata = pmartRdata::metab_fdata,
#'                                edata_filename = "metab_edata",
#'                                fdata_filename = "metab_fdata")
#'                                
#' # Make a metabolomics NMR project
#' nmr_project <- project_pmart(projectname = "My NMR Data",
#'                                datatype = "Metabolomics-NMR",
#'                                edata = pmartRdata::nmr_edata_identified,
#'                                fdata = pmartRdata::nmr_fdata_identified,
#'                                emeta = pmartRdata::nmr_emeta_identified,
#'                                edata_filename = "nmr_edata",
#'                                fdata_filename = "nmr_fdata",
#'                                emeta_filename = "nmr_emeta")
#'                           
#'                                                     
#' # Make a lipidomics project
#' lipid_project <- project_pmart(projectname = "My Lipid Data",
#'                                datatype = "Lipidomics-Positive",
#'                                edata = pmartRdata::lipid_edata,
#'                                fdata = pmartRdata::lipid_fdata,
#'                                edata_filename = "lipid_edata",
#'                                fdata_filename = "lipid_fdata")
#'                           
#' # Finally, make the ipmart midpoint object
#' project_ipmart(objects = list(midpoint, metab_project, nmr_project, lipid_project))                          
#' 
#' }
#' @export
project_ipmart <- function(objects) {
  
  # Check the length of the object. It must be between 2 and 5.
  if (length(objects) > 1 & length(objects) < 6) {
    stop("objects must have at least 2 objects and no more than 6.")
  }
  
  # All objects must have pmart in them
  if ((grepl("pmart", lapply(objects, class) %>% unlist()) %>% all()) == FALSE) {
    stop("objects must all be pmart projects or midpoints.")
  }
  
  # Get project types 
  ProjectTypes <- lapply(objects, function(object) {
    
    # Extract the object type
    if (class(object) == "midpoint pmart") {
      DataType <- object$Tracking$`Original Files`$Project$DataType
    } else if (class(object) == "project pmart") {
      DataType <- object$Project$DataType
    }
    
    # Simplify data type to metabolomics/lipidomics/proteomics
    if (grepl("Peptide|Protein", DataType)) {return("Proteomics")} else
    if (grepl("Lipidomics", DataType)) {return("Lipidomics")} else {return(DataType)}
    
  }) %>% unlist()
  
  
}



