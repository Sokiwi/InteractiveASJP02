MEGA_to_simple <- function(mega_file, user_file) {
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
	x <- x[-1]
	writeLines(x, con="simple_tmp.txt")
	rm(x)
	m1 <- scan("simple_tmp.txt", what="character", quiet=TRUE)
	invisible(file.remove("simple_tmp.txt"))
	tnnum <- as.numeric(tn)
	wt <- seq(1, tnnum^2, by=tnnum+1)
	taxa <- m1[wt]
	m2 <- m1[-wt]
	rm(m1)
	m3 <- matrix(m2, ncol=tnnum, nrow=tnnum, byrow=TRUE)
	rm(m2)
	cat("Language_A\tLanguage_B\tLDND\n", file=user_file)
	for (i in 1:(length(taxa)-1)) {
		for (j in (i+1):(length(taxa))) {
			cat(taxa[i], "\t", taxa[j], "\t", m3[j,i], "\n", file=user_file, append=TRUE)
		}
	}
}
