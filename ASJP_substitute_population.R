substitute_population <- function(string, oldpop, newpop) {
	spl <- strsplit(string, "")[[1]]
	spaces <- rep(" ", 11 - nchar(newpop))
	newpop_plus_spaces <- c(spaces, strsplit(as.character(newpop), "")[[1]])
	spl_new <- c(spl[1:19], newpop_plus_spaces, spl[31:length(spl)])
	return(paste(spl_new, collapse=""))
}
