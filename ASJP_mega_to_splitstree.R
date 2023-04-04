MEGA_to_splitstree <- function(mega_file, splitstree_file) {
	x <- readLines(mega_file)
	ltn <- strsplit(x[5], " ")  # stands for line with taxon number
	tn <- ltn[[1]][length(ltn[[1]])]
	get_name <- function(s) {
		return(strsplit(s, "\\#")[[1]][2])
	}
	names <- as.vector(unlist(lapply(x[12:(11+as.numeric(tn))], get_name)))
	x <- x[-c(1:7)]
	x[1] <- "#nexus"
	x[2] <- "BEGIN Taxa;"
	x[3] <- paste("DIMENSIONS ntax=", tn, ";", sep="")
	x[4] <- "TAXLABELS"
	for (i in 5:(4+as.numeric(tn))) {
		x[i] <- gsub("#", "'", x[i])
		x[i] <- paste(x[i], "'", sep="")
	}
	first_end <- 4 + as.numeric(tn)
	last_start <- 4 + as.numeric(tn) + 4
	last_end <- 4 + as.numeric(tn) + 3 + as.numeric(tn)
	x <- c(x[1:first_end], ";", "END; [Taxa]", "", "BEGIN Distances;", paste("DIMENSIONS ntax=", tn, ";", sep=""), "FORMAT labels=left diagonal triangle=lower;", "MATRIX", x[last_start:last_end], ";", "END; [Distances]", "")
	ms <- last_start + 4  # stands for matrix start
	me <- last_end + 4  # stands for matrix end
	x[ms] <- paste(x[ms], "  ", sep="")
	for (j in ms:me) {
		x[j] <- paste(strsplit(x[j], "]")[[1]][1], "] ", "'", names[j-(ms-1)], "'", strsplit(x[j], "]")[[1]][2], "0.0", sep="")
	}
	writeLines(x, splitstree_file)
}
