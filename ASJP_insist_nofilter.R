insist_nofilter <- function(input) {
	input <- gsub(" ", "", input)
	input <- gsub(",", "", input)
	input <- gsub("-", "", input)
	input <- gsub(";", "", input)
	while ( length(grep("0", input)) > 0 & nchar(input) > 1 ) {
		cat("\nYou need to decide whether or not you are applying a filter.\n")
		cat("\nPlease try again here: ")
		input <- readline()
		input <- gsub(" ", "", input)
		input <- gsub(",", "", input)
		input <- gsub("-", "", input)
		input <- gsub(";", "", input)
	}
	return(input)
}
