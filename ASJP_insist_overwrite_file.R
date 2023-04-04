insist_overwrite_file <- function(input) {
	while ( tolower(input) != "x" & input %in% dir()==TRUE ) {
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
