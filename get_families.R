

# pseudo
get_family <-  function(package) {
  
  return(list(list("package" = "stats", "family"= "uni"),list("package" = "stats", "family"= "uni"),list("package" = "stats", "family"= "uni")))
  
}

# pseudo
get_params <- function(package.familiy) {
  
  return(list("hi"=3,"hdsafi"=3,"hadfi"=3,"hasdfi"=3))
}

search <- function(all.packages) {
  
  for (i in 1:len(all.packages)) {
    
    package.families <- get_family(all.packages[i])  
    
    for (j in 1:len(package.families)) {
      
      get_params
      
    }
    
      
    
    
    
  }
  
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