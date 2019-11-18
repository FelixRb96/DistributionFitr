res <- iterate_packages(c("stats"))
res

families <- construct_package_list(all.packages = FALSE)
families

res <- iterate_packages(families)

all_pkgs <- construct_package_list(all.packages = TRUE)
res_all <- iterate_packages(all_pkgs)

getFamilies(all.packages = c("stats"), file = "all_families.R")


getFamilies()
