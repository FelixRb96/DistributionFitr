getwd()

files <- list.files('../private/results/Rout')
packages <- c()
distributions <- list()
j <- 1
for(i in 1:length(files)) {
  a <- dget(paste0('../private/results/Rout/', files[i]))
  if(length(a)!=0) {
    packages[[j]] <- unlist(strsplit(files[i], split = '.R'))[1]
    distributions[(length(distributions)+1):(length(distributions) + length(a))] <- a
    j <- j+1
  }
}

install.packages(packages[!(packages %in% rownames(installed.packages()))])

for(i in 1:length(packages)) {
  tryCatch(suppressMessages(eval(parse(text=paste0('library(', packages[i],')')))),
           error = function(x) warning("Library '", packages[i], "'not available")
  )
}

dput(distributions,file='all_families.R')
