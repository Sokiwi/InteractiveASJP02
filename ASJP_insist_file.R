insist_file <- function(input) {
	if ( tolower(input) != "x" ) {
		while ( input %in% dir()==FALSE ) {
			cat("\n", input, "is not in the working directory\n")
			cat("\nPlease try again here: ")
			input <- readline()
		}
	}
	return(input)
}
