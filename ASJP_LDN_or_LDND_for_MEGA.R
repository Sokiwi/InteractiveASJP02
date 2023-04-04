# make MEGA-style LDN or LDND matrix
# runs as for instance MEGA_format("LDND", "testout.meg", names, data_words)
MEGA_format <- function(LDN_or_LDND, outfile, names, data_words) {
	source("ASJP_LDN.R")
	source("ASJP_LDND.R")
	L <- length(names)
	cat("#mega\n", file=outfile)
	cat("!Title: Concatenated Files;\n", file=outfile, append=TRUE)
	cat("!Format DataType=Distance DataFormat=LowerLeft NTaxa=  ", L, ";\n", sep="", file=outfile, append=TRUE)
	cat("!Description\n", file=outfile, append=TRUE)
	cat("  No. of Taxa :  ", L, "\n", file=outfile, append=TRUE)
	cat("  No. of Groups :  1\n", file=outfile, append=TRUE)
	cat("  Gaps/Missing data : Pairwise Deletion\n", file=outfile, append=TRUE)
	cat("  Codon Positions : 1st+2nd+3nd+Noncoding\n", file=outfile, append=TRUE)
	cat("  Distance method : Nucleotide: Tamura-Nei  [Pairwise distances]\n", file=outfile, append=TRUE)
	cat("  d : Estimate\n", file=outfile, append=TRUE)
	cat(";\n", file=outfile, append=TRUE)
	for (i in 1:L) {
		if ( i < 10 ) {
			spaces <- "   "
		}
		if ( i >= 10 & i < 100 ) {
			spaces <- "  "
		}
		if ( i >= 100 & i < 1000 ) {
			spaces <- " "
		}
		cat(paste("[", spaces, i, "] #", names[i], "\n", sep=""), file=outfile, append=TRUE)
	}
	cat("\n", file=outfile, append=TRUE)
	cat("[     ", file=outfile, append=TRUE)
	cat(seq(1:L), sep="   ", file=outfile, append=TRUE)
	cat("\n", file=outfile, append=TRUE)
	cat("]\n[   1]\n", file=outfile, append=TRUE)
	for (j in 2:L) {
		if ( j < 10 ) {
			spaces <- "   "
		}
		if ( j >= 10 & j < 100 ) {
			spaces <- "  "
		}
		if ( j >= 100 & j < 1000 ) {
			spaces <- " "
		}
		cat(paste("[", spaces, j, "]  ", sep=""), file=outfile, append=TRUE)
		# compute the LDNs or LDNDs and put them in a file,
		for (k in 1:(j-1)) {
			if ( LDN_or_LDND=="LDN" ) {
				ldn <- round(LDN(names[j], names[k], data_words), 4)
				ldn <- as.character(ldn)
				n_length <- nchar(ldn)
				add_zeros <- 6 - n_length
				if ( add_zeros > 0 & add_zeros < 5 ) {
					zeros <- paste(rep("0", add_zeros), collapse="")
					ldn <- paste(ldn, zeros, sep="")
				}
				if ( add_zeros == 5 ) {
					ldn <- paste(ldn, ".0000", sep="")
				}
				cat(paste(ldn, "  ", sep=""), file=outfile, append=TRUE)
			}
			if ( LDN_or_LDND=="LDND" ) {
				ldnd <- round(LDND(names[j], names[k], data_words), 4)
				ldnd <- as.character(ldnd)
				n_length <- nchar(ldnd)
				add_zeros <- 6 - n_length
				if ( add_zeros > 0 & add_zeros < 5 ) {
					zeros <- paste(rep("0", add_zeros), collapse="")
					ldnd <- paste(ldnd, zeros, sep="")
				}
				if ( add_zeros == 5 ) {
					ldnd <- paste(ldnd, ".0000", sep="")
				}
				cat(paste(ldnd, "  ", sep=""), file=outfile, append=TRUE)
			}
		}
		cat("\n", file=outfile, append=TRUE)
	}
}

