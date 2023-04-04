# lists_s is a list of wordlists
# this function returns "no overlap" first time it meets
# a pair where the overlap is zero
# if this never happens it returns "overlap"
zero_overlap <- function(lists_s) {
	for (p in 1:(length(lists_s) - 1)) {
		for (q in 2:length(lists_s)) {
			if ( !is.na(lists_s[[p]][1]) & !is.na(lists_s[[q]][1]) ) {
				miss <- unique(c(which(lists_s[[p]]=="XXX"), which(lists_s[[q]]=="XXX")))
				if ( length(miss)==40 ) {
					return("no_overlap")
				}
			}
		}
	}
	return("overlap")
}
