source("get_params.R")


#iterate over packages and extract families and params
iterate_packages <- function(packages) {
  
  df <- data.frame("family"= c(), package = c(), params = c())
  
  #iterate over packages
  for (i in 1:length(packages)) {
    package_content <- get_family(packages[i])
    
    #iterate over all families withing package
    for (j in 1:length(package_content[j])) {
      #
      stopifnot(package_content[j]["package"] == packages[i])
      
      
      #fetch params for each family and add to df
      params <- get_params(package_content[j])
      
      df <- rbind(df, c(package_content[j]["family"], 
                        package_content[j]["package"],
                        params))
    }
  }
  
  return(df) 
  
}


## internal
construct_package_list <- function(all.packages) {
 
  if (all.packages == TRUE)
  {
    all.packages <- as.vector(installed.packages()[,"Package"])
    
  } else if (all.packages == FALSE) 
  {
    #only select "base" and "recommended" packages
    ins_pck <- installed.packages()
    
    ## boolean value of what to keep
    package_filter <- ((ins_pck[,"Priority"] == "base" | ins_pck[,"Priority"] == "recommended") & !is.na(ins_pck[,"Priority"]))
    
    all.packages <- as.vector(ins_pck[package_filter,"Package"])
    
    ##neccessary?
    rm(ins_pck)
  }
  
  return(all.packages)

  ##Tim
  
}

write_file <- function(list, file) {
  ###save list as txt
  ##Adrian
}

get_families <- function() {
  #read .txt and return list of lists
  
}