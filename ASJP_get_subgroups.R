get_subgroups <- function(w, classification) {
	uni <- unique(classification[w])
	split_comma <- function(x) {
		comma_present <- grep(",", x)
		if ( length(comma_present) > 0 ) {
			subgroup <- strsplit(x, ",")[[1]][2]
			return(subgroup)
		} else {
			return(NA)
		}
	}
	out <- as.vector(unlist(lapply(uni, split_comma)))
	out <- unique(out[!is.na(out)])
	if ( length(out) > 0 ) {
		return(out)
	} else {
		return(NA)
	}
}	
