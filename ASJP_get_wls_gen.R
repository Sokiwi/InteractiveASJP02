get_wls_gen <- function(x) {
	x1 <- strsplit(x, "\\{")[[1]][2]
	x2 <- strsplit(x1, "\\|")[[1]][1]
	x3 <- strsplit(x2, "\\.")[[1]][2]
	return(x3)
}
