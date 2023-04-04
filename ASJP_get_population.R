get_population <- function(x) {
	x1 <- strsplit(x, "")
	if ( length(x1[[1]]) >= 30 ) {
		popstr1 <- x1[[1]][c(20:30)]
		popstr2 <- paste(popstr1, collapse="")
		popstr3 <- trimws(popstr2)
		if ( nchar(popstr3) >= 1 ) {
			return(as.numeric(popstr3))
		} else {
			return(NA)
		}
	} else {
		return(NA)
	}
}
