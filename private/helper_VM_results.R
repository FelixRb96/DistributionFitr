##### import vm results and write them in one file must be currently executed in
##### testing branch (where the files are saved)
files <- list.files("../private/results/Rout")
packages <- c()
distributions <- list()
j <- 1
for (i in 1:length(files)) {
  a <- dget(paste0("../private/results/Rout/", files[i]))
  if (length(a) != 0) {
    packages[[j]] <- unlist(strsplit(files[i], split = ".R"))[1]
    distributions[(length(distributions) + 1):(length(distributions) + length(a))] <- a
    j <- j + 1
  }
}
dput(distributions, file = "all_families.R")
save(packages, file = "private/relevant_packages.Rda")
