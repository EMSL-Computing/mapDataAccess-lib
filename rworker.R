#!/usr/bin/env Rscript  
library(rworker)
library(reticulate)

print("INFO:  New Rworker thread started...")
reticulate::use_virtualenv("/venv")

# Register url, this is running in another docker container alongside this one
if(!file.exists("redis_config.yml")){
  warning("No redis configuration found, attempting connection to default url: redis://redis1:6379")
  redis_url <- "redis://redis1:6379"
} else {
  redis_cfg = yaml::read_yaml("redis_config.yml")
  redis_host = if(Sys.getenv("SHINY_LOCAL_OR_NETWORK")=="local") "0.0.0.0" else redis_cfg[['host']]
  redis_url <- sprintf('redis://:%s@%s:%s/%s', 
                       redis_cfg[["password"]], 
                       redis_host, 
                       redis_cfg[['port']],
                       redis_cfg[['db']])
}

message("Setting up redis connection at:  ", redis_url)
# Instantiate Rworker object --> link between worker and task manager
consumer <- rworker(name = 'celery', workers = 1, queue = redis_url, backend = redis_url)

edata_simple_boxplots <- function(object_name, id, panel_column, groups){
  library(trelliscopejs)
  library(mapDataAccess)
  library(readr)
  library(reticulate)
  library(dplyr)
  library(plotly)
  
  source("preprocessing/edata_preprocessing.R")
  source("plot_functions/boxplots.R")
  
  miniocon = mapDataAccess::map_data_connection(config_file='minio_config.yml')

  # Start progress message 
  task_progress(sprintf(
    "INFO:  Creating simple boxplot display from %s.  tag: %s", object_name, id))
  
  # Read in using minio, can the server see the minio object?
  projectObject = mapDataAccess::get_data(miniocon, object_name)
  
  edata <- projectObject$Data$e_data
  
  nested_edata <- edata_to_plot_df(
    edata,
    panel_column = panel_column,
    names_to = "Sample",
    values_to = "Value"
  )
  
  # TODO:  Function takes grouping information, passed to redis
  nested_edata <- nested_edata %>% dplyr::mutate(data = purrr::map(data, function(df){
    df[['__GROUP_COL__']] <- groups
    
    return(df)
  }))
  
  nested_edata <- nested_edata %>% dplyr::mutate(
    plot = pmap_plot(
      list(
        df = data,
        group_cname = "__GROUP_COL__",
        value_cname = "Value"
      ),
      simple_boxplots
    ),
    identifier = trelliscopejs::cog(!!rlang::sym(panel_column), 
                     desc = "Biomolecule", 
                     default_label = T)
  ) %>% dplyr::ungroup()
  
  trelliscope(nested_edata, 
              name = "Biomolecule boxplots", 
              path = id
  ) %>% {
    invisible(print(., view = FALSE))
  }
  
  task_progress("INFO:  Display creation finished")
  
  # this may change in the future
  fileprefix = id
  
  ## Manually fix bug where the index file does not point to the correct location ##
  
  index_path <- file.path(fileprefix, 'index.html')
  temp_index <- readLines(index_path)
  # leading slashes mess things up, so included [/|\\]* regex, please change if there is a better way to do this substitution
  temp_index <- gsub(file.path(paste0('[/|\\]*', id), 'appfiles'), 'appfiles', temp_index)
  fileConn <- file(index_path)
  writeLines(temp_index, fileConn)
  close(fileConn)
  ##
  
  # Dump files to minio with the id as a reference
  allfiles = list.files(fileprefix, recursive=TRUE, include.dirs = F)
  for(f in allfiles){
    miniocon$client$fput_object(miniocon$bucket, 
                                object_name = file.path(paste0("trelli-display-", id), f), 
                                file_path = file.path(fileprefix, f))
  }
  
  task_progress(sprintf(
    "INFO:  Display written to minio storage with prefix: %s", paste0("trelli-display-", id)))
  
}

# Register the task with redis
consumer$task(edata_simple_boxplots, name = "edata_simple_boxplots")

# Om-nom-nom
consumer$consume()
