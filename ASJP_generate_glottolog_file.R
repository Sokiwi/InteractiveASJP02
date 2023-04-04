## This script generates a file from Glottolog
gfile <- function() {
	if ( ("stringi" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("stringi", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	library(stringi)
	# put it in in an appropriate folder, like in next line, but adapt that line
	g <- read.csv(file="languoid.csv", sep=",", encoding="UTF-8")

	# generate a file with the headers "ISO639-3", "Classification"

	## functions used in generating the file

	# function for creating a pseudo-code when ISO-code is missing
	NOCODEstring <- function(s) {
		s1 <- gsub(" ", "", s)
		s2 <- stri_trans_general(s1, "Latin-ASCII")
		s3 <- paste("NOCODE_", s2, sep="")
		return(s3)
	}

	# function for formatting a classification string
	# changing it to ascii and taking out the last comma
	class_string_format <- function(s) {
		s1 <- gsub(" ", "", s)
		s2 <- stri_trans_general(s1, "Latin-ASCII")
		s3 <- strsplit(s2, ",")[[1]]
		s4 <- paste(s3, collapse=",")
		return(s4)
	}

	# function for composing a classification string
	class_string <- function(ind) {
		ancestors <- ""
		parent_id <- "master_Wu"
		while ( parent_id != "" ) {
			parent_id <- g$parent_id[ind]
			if ( parent_id != "" ) { 
				w <- which(g$id==parent_id)
				ancestors <- paste(g$name[w], ",", ancestors, sep="")
				ind <- w
			}
		}
		if ( ancestors == "" ) {
			ancestors <- g$name[ind]
		}
		return(ancestors)
	}
	
	# routine for composing a classification string for each language
	# including ISO-code languages considered dialects in Glottolog
	cat("ISO639-3\tClassification\n", file="glottolog.tab")
	for (i in 1:nrow(g)) {
		if ( g$level[i]=="language" | nchar(g$iso639P3code[i])==3 ) {
			iso <- g$iso639P3code[i]
			if ( iso == "" ) {
				iso <- NOCODEstring(g$name[i])
			}
			clstr <- class_string(i)
			clstrf <- class_string_format(clstr)
			cat(iso, "\t", clstrf, "\n", sep="", file="glottolog.tab", append=TRUE)
			# cat(iso, "\t", clstrf, "\n")
		}
	}
}
