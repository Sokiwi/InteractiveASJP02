get_e_fam <- function(x) {
	x1 <- strsplit(x, "\\|")[[1]][2]
	x2 <- strsplit(x1, "\\@")[[1]][1]
	x3 <- strsplit(x2, ",")[[1]][1]
	return(x3)
}
