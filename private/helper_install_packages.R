# Installing required packages 

load('private/relevant_packages.Rda')
# install required packaged
install.packages(packages[!(packages %in% rownames(installed.packages()))])

# load required packages
for(i in 1:length(packages)) {
  tryCatch(suppressMessages(eval(parse(text=paste0('library(', packages[i],')')))),
           error = function(x) warning("Library '", packages[i], "'not available")
  )
}



