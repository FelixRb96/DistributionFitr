## Authors 
## Tim Glockner, tim.glockner@outlook.com
##
## Extract distribution families along with infos regarding their parameters from
## multiple R packages
##
## Copyright (C) 2019 -- 2020 Tim Glockner
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  


#iterate over packages and extract families and params
iterate_packages <- function(packages) {
  
  res <- list()
  
  #iterate over packages
  for (i in 1:length(packages)) {
    ## package <- packages[i]
    cat("Current Package:", packages[i])
    package_content <- getFamily(packages[i])
    cat("\tNumber of families:", length(package_content), "\n")
    
    # no distribution family contained in package
    if (length(package_content) == 0) next
    
    #iterate over all families withing package
    for (j in 1:length(package_content)) {
      #
      stopifnot(package_content[[j]]$package == packages[i])
      
      cat("Current Family:", package_content[[j]]$family, "\n")
      #fetch params for each family and add to 
      params <- tryCatch(getParams(package_content[[j]]),
                         error = function(e) {
                           message("Error occured for family ",
                                   package_content[[j]]$family,
                                   "\n", e)
                           ## message(e, "\n")
                           NULL
                         })
      if (!is.null(params)) ## length(params) != 0 ## semantisch das richtige?
        res[[length(res) + 1]] <- c(package_content[[j]],
                                    list(family_info = params))
    }
  }
  
  return(res) 
  
}


## internal
construct_package_list <- function(all.packages) {
  
  if (all.packages == TRUE) ## if (all.packages)
  {
    all.packages <- as.vector(installed.packages()[,"Package"])
    
  } else
    if (all.packages == FALSE) ## (i) if (!all.packages)... waere besser
    ##                                (ii) ganz weglassen! (Alternativen zu
    ##                                     TRUE / FALSE ?)
  {
    #only select "base" and "recommended" packages
    ins_pck <- installed.packages()
    
    ## boolean value of what to keep
    package_filter <- ((ins_pck[,"Priority"] == "base" |
                        ins_pck[,"Priority"] == "recommended")
      & !is.na(ins_pck[,"Priority"]))
    
    all.packages <- as.vector(ins_pck[package_filter,"Package"])
    
    rm(ins_pck)
  }
  
  return(all.packages)
  
  
}


write_file <- function(family_list, file="all_families.rds") {
  saveRDS(family_list, file=file)
}


# Case 1: all.packages gegeben: Suche nach Verteilungsfamilien
# Case 1.1 vector of strings (Liste von Packetnamen) -> take families from those packages
# Case 1.2 FALSE -> only take recommended / base packages
# Case 1.3 TRUE -> take all installed packages
# Case 2 all.packages missing: Take families saved in the file

getFamilies <- function(all.packages, file="R/all_families.rds") {
  # CASE 2:
  if (missing(all.packages)) {
    if (! (file %in% list.files()) ) ## das passt mit dem default
      ## von file nicht zusammen
      getFamilies(all.packages = FALSE, file=file) ## warum nicht return(...) ?
    #read file and return list of lists
    family_list <- readRDS(file=file)
    return(family_list)
  }

  ## CASES 1.3 und 1.2 ist Code massiv gedoppelt. Bitte is.logical verwenden
  ## und die Zeilenzahl halbieren.
  # CASE 1.3
  if (length(all.packages) == 1 && isTRUE(all.packages)) {
    ## doppelt gemoppelt. S. Def von isTRUE
    family_list <- iterate_packages(construct_package_list(all.packages = TRUE))
    write_file(family_list=family_list,file="R/all_families.rds")## Konstanten
    ## mitten im Code darf nicht sein!!
    return(family_list)
  }
  
  # CASE 1.2
  if (length(all.packages) == 1 && isFALSE(all.packages)) {
    ## doppelt gemoppelt. S. Def von isFALSE
    family_list <- iterate_packages(construct_package_list(all.packages = FALSE))
    write_file(family_list=family_list,file="R/all_families.rds")
    return(family_list)
  }
  
  # CASE 1.1
  family_list <- iterate_packages(all.packages)
  write_file(family_list=family_list,file="R/all_families.rds")
  return(family_list)
}
