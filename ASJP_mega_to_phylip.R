MEGA_to_phylip <- function(mega_file, outfile="phylip_in.txt") {
	x <- readLines(mega_file)
	ltn <- strsplit(x[5], " ")  # stands for line with taxon number
	tn <- ltn[[1]][length(ltn[[1]])]
	get_name <- function(s) {
		return(strsplit(s, "\\#")[[1]][2])
	}
	names_file <- as.vector(unlist(lapply(x[12:(11+as.numeric(tn))], get_name)))
	before_matrix <- 11 + as.numeric(tn) + 3
	x <- x[-c(1:before_matrix)]
	x[1] <- ""
	for (i in 2:length(x)) {
		x[i] <- strsplit(x[i], "\\]  ")[[1]][2]
		x[i] <- strsplit(x[i], "  $")[[1]][1]
	}
	L <- list()
	for (j in 1:length(x)) {
		L[[j]] <- strsplit(x[j], "  ")[[1]]
	}
	for (k in 1:length(L)) {
		L[[k]] <- c(L[[k]], "0.0000")
	}
	for (l in 1:(length(L)-1)) {
		for (m in (l+1):length(L)) {
			L[[l]][m] <- L[[m]][l]
		}
	}
	for (n in 1:length(L)) {
		x[n] <- paste(L[[n]], collapse="  ")
	}
	taxon_no_line <- paste("     ", tn, sep="")
	x <- c(taxon_no_line, x)
	# from here deal with adding names
	format_names <- function(n, ln) {
		ln <- max(as.vector(unlist(lapply(names_file, nchar))))  # stands for longest_name
		fixed_total <- ln + 1
		spaces_added <- fixed_total - nchar(n)
		space_string <- paste(rep(" ", spaces_added), collapse="")
		n_formatted <- paste(n, space_string, sep="")
		return(n_formatted)
	}
	names_form <- as.vector(unlist(lapply(names_file, format_names)))
	for (o in 2:length(x)) {
		x[o] <- paste(names_form[o-1], x[o], sep="")
	}	
	writeLines(x, con=outfile)
}
