get_fams_non_unique <- function(classification) {
	split_comma <- function(x) {
		comma_present <- grep(",", x)
		if ( length(comma_present) > 0 ) {
			famname <- strsplit(x, ",")[[1]][1]
		} else {
			famname <- x
		}
	}
	return(as.vector(unlist(lapply(classification, split_comma))))
}
