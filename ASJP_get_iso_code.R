get_iso_code <- function(x) {
	x1 <- strsplit(x, "")
	if ( length(x1[[1]]) == 42 ) {
		isostr1 <- x1[[1]][c(40:42)]
		isostr2 <- paste(isostr1, collapse="")
		iso <- trimws(isostr2)
		if ( nchar(iso) == 3 | nchar(iso) == 2 ) {
			return(iso)
		} else {
			return(NA)
		}
	} else {
		return(NA)
	}
}

