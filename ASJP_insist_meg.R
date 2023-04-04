insist_meg <- function(input) {
	while ( tolower(input) != "x" & (length(grep("\\.meg$", input))==0 | input %in% dir()==TRUE) ) {
		if ( length(grep("\\.meg$", input))==0 ) {
			cat("\nYour file name does not carry the suffix .meg.\n")
			cat("\nPlease type another name here: ")
			input <- readline()
		}
		if ( input %in% dir()==TRUE & length(grep("\\.meg$", input))!=0 )
			cat("\nThere is already a file with that name.\n")
			cat("Do you want to\n")
			cat("    1. overwrite it?\n")
			cat("    2. choose another file name?\n\n")
			cat("Type a number and press ENTER ")
			optOVERWRITE <- readline()
			if ( optOVERWRITE=="1" ) {
				return(input)
			} else { 
				cat("\nPlease type another name here: ")
				input <- readline()
			}
	}
	return(input)
}
