# source all functions contained in the package execute for example before
# testing
library(stringr)

for (file in list.files("R/")) {
  if (file == "source_all.R") {
    next
  }
  if (str_sub(file, -2, -1) != ".R") {
    next
  }
  # cat(file, '\n')
  source(paste0("R/", file))
}

# getFamilies(T)
