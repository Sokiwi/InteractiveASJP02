MEGA_to_matrix <- function(mega_file, outfile="matrix.txt") {
	x <- readLines(mega_file)
	ltn <- strsplit(x[5], " ")  # stands for line with taxon number
	tn <- ltn[[1]][length(ltn[[1]])]
	get_name <- function(s) {  # function to get individual names
		return(strsplit(s, "\\#")[[1]][2])
	}
	names_file <- as.vector(unlist(lapply(x[12:(11+as.numeric(tn))], get_name)))
	before_matrix <- 11 + as.numeric(tn) + 3
	x <- x[-c(1:before_matrix)]
	x[1] <- ""
	# get rid of the colum to the left with the numbers
	for (i in 2:length(x)) {
		x[i] <- strsplit(x[i], "\\]  ")[[1]][2]
		x[i] <- strsplit(x[i], "  $")[[1]][1]
	}
	# put each row into one and the same list
	L <- list()
	for (j in 1:length(x)) {
		L[[j]] <- strsplit(x[j], "  ")[[1]]
	}
	# add 0.0000 to the diagonal
	for (k in 1:length(L)) {
		L[[k]] <- c(L[[k]], "0.0000")
	}
	# make the matric symmetrical
	for (l in 1:(length(L)-1)) {
		for (m in (l+1):length(L)) {
			L[[l]][m] <- L[[m]][l]
		}
	}
	# make the lines tab-delimited and put them back into the object x
	# replacing what was there
	for (n in 1:length(L)) {
		x[n] <- paste(L[[n]], collapse="\t")
	}
	# add names
	for (o in 1:length(x)) {
		x[o] <- paste(names_file[o], "\t", x[o], sep="")
	}
	cat(names_file, sep="\t", file=outfile)
	cat("\n", file=outfile, append=TRUE)
	for (p in 1:length(x)) {
		cat(x[p], "\n", file=outfile, sep="", append=TRUE)
	}
	# this can subsequently be read in using: 
	# m <- read.table(file="matrix.txt", header=TRUE, row.names=1)
}


