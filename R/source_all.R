# source all functions contained in the package
# execute for example before testing
for (file in list.files("R/")) {
  if (file == "source_all.R") next
  cat(file, "\n")
  source(paste0("R/", file))
}
