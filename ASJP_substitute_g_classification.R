substitute_g_classification <- function(string, new_g_cls) {
	before <- strsplit(string, "@")[[1]][1]
	return(paste(before, "@", new_g_cls, "}", sep=""))
}

