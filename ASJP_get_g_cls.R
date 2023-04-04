get_g_cls <- function(x) {
	x1 <- strsplit(x, "@")[[1]][2]
	x2 <- strsplit(x1, "\\}")[[1]][1]
	return(x2)
}
