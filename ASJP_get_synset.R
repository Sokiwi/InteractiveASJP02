get_synset <- function(x) {
	x1 <- strsplit(x, "\t")[[1]][2]
	x2 <- trimws(strsplit(x1, "//")[[1]][1])
	return(x2)
}
