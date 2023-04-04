insist_twodigits <- function(input) {
	expect <- c("12", "13", "14", "15", "16", "17", "18")
	input <- gsub(" ", "", input)
	input <- gsub(",", "", input)
	input <- gsub("-", "", input)
	input <- gsub(";", "", input)
	w_unexpect <- which(is.na(match(input, expect)))
	cases <- input[w_unexpect]
	while ( length(w_unexpect) > 0 ) {
		cat("\n", cases, "is unexpected input\n")
		cat("\nPlease try again here: ")
		input <- readline()
		input <- gsub(" ", "", input)
		input <- gsub(",", "", input)
		input <- gsub("-", "", input)
		input <- gsub(";", "", input)
		w_unexpect <- which(is.na(match(input, expect)))
		cases <- input[w_unexpect]
	}
	return(input)
}
