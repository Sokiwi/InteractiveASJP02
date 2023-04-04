capitalize <- function(x) {
	s <- strsplit(x, "")
	s[[1]][1] <- toupper(s[[1]][1])
	w <- which(s[[1]]=="-")
	if ( length(w) > 0 ) {
		for (i in 1:length(w)) {
			if ( w[i] < length(s[[1]]) ) {
				s[[1]][w[i]+1] <- toupper(s[[1]][w[i]+1])
			}
		}
	}
	return(paste(s[[1]], collapse=""))
}
