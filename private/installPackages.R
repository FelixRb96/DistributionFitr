# Description:
# this script was used to install most of the available packages from cran


ap <- available.packages()
# ret <- vector(mode='integer', length=nrow(ap))


library(devtools)


ret <- list()

logFile <- 'log.txt'


writeLog <- function(text, ...){
        write(sprintf(text, ...), file=logFile, append=TRUE)
}

isInstalled <- function(pkg){
        ip <- .packages(all.available=TRUE)
        return(pkg %in% ip)
}

# pkgList <- scan('keyPackages.txt', what='', sep='\n')
# pkgList <- scan('missingPackages.txt', what='', sep='\n')
# nPkg <- length(pkgList)
nPkg <- nrow(ap)
writeLog('Installing %d packages', nPkg)


for(i in 1:nPkg){
# for(i in 1:5){
# for(i in 6430:nPkg){
# i <- 0
# for(pkg in pkgList){
        # i <- i + 1
        pkg <- ap[i,'Package']
        writeLog('')
        writeLog(sprintf('%d/%d', i, nPkg))
        writeLog(pkg)
        if(isInstalled(pkg)){
                ret[[pkg]] <- -1
                writeLog('already installed')
        } else{
                writeLog('installing...')
                # install.packages(pkg)
		try(
			devtools::install_version(pkg)
		)

                if(isInstalled(pkg)){
                        writeLog('installed successfully')
                        ret[[pkg]] <- 0
                } else{
                        writeLog('error')
                        ret[[pkg]] <- 1
                }
        }
}

saveRDS(ret, 'ret.RDS')
