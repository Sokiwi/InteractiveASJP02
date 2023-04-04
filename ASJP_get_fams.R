get_fams <- function(classification) {
	uni <- unique(classification)
	split_comma <- function(x) {
		comma_present <- grep(",", x)
		if ( length(comma_present) > 0 ) {
			famname <- strsplit(x, ",")[[1]][1]
		} else {
			famname <- x
		}
	}
	return(unique(as.vector(unlist(lapply(uni, split_comma)))))
}
