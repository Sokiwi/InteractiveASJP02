get_doculect <- function(x) {
	x1 <- strsplit(x, "\\{")[[1]][1]
	return(x1)
}
