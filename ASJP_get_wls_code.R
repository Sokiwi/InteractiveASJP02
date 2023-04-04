# extracts the wals code from the second line of metadata
get_wls_code <- function(x) {
	x1 <- strsplit(x, "")[[1]]
	if ( length(x1) >= 36 ) {
		wals_code <- trimws(paste(x1[34:36], collapse=""))
		if ( nchar(wals_code) == 3 ) {
			return(wals_code)
		} else {
			return(NA)
		}
	} else {
		return(NA)
	}
}
