# Test for getFamily function

package <- "stats"
possible_dists <- lsf.str(paste0("package:", package), pattern = "^[rdpq]") # all functions starting with r, d, p or q
possible_dists
start_chars <- c("d", "p", "q", "r")
first_args <- c("x", "q", "p", "n") # first parameters of the d, p, q, r functions
l <- list()
for (i in 1:length(start_chars)) {
  char <- start_chars[i]
  first_arg <- first_args[i]
  subset <- grep(paste0("^", char), possible_dists, value = TRUE) # all functions starting with char
  valid_idx <- sapply(subset, function(x) names(as.list(args(x)))[1] == first_arg) # check if all functions have the correct first arg
  # print(valid_idx)
  l[[char]] <- subset[valid_idx]
}

get_endings <- function(vec) str_sub(vec, start = 2)

l_endings <- lapply(l, get_endings) # remove the d, p, q, r suffixes

# we definitely need a function for the density starting with d, as otherwise we
# cannot evaluate likelihood function so we only take the endings from p, q and r
# that also appear in d
for (char in start_chars[-1]) {
  l_endings[[char]] <- intersect(l_endings[[char]], l_endings$d)
}

freq <- table(unlist(l_endings)) # get a frequency table of the endings
freq <- freq[freq >= 2] # only take those distributions that have at least 2 functions implemented
freq

# drop some not fitting distributions
to_drop <- c("multinom")
families <- names(freq)[!(names(freq) %in% to_drop)]

# list of lists, where each sublist has the form list(package=some_pkg,
# family=some_family)
families <- lapply(families, function(x) list(package = package, family = x))
families
