# res <- iterate_packages(c("stats"))
# res
# 
# families <- construct_package_list(all.packages = FALSE)
# families
# 
# res <- iterate_packages(families)
# 
# all_pkgs <- construct_package_list(all.packages = TRUE)
# res_all <- iterate_packages(all_pkgs)

###
# setwd("/home/manuel/RProjekt/DistributionFitr/private")
source('../R/source_all.R')

# l <- installed.packages()
# removePkgs <- c('actuar', 'COMPoissonReg', 'datos')
# removePkgs <- c()
# l <- l[!row.names(l) %in% removePkgs,]
# pkgList <- l[1:dim(l)[1],1]
# pkgList = c('datos')
###

pkgList <- commandArgs(trailingOnly=T)

if(length(pkgList)>0){
    fileName <- paste0('./Rout/', paste(pkgList, collapse='_'), '.R')
    getFamilies(all.packages = pkgList, file = fileName)
}

cat('Done.')
