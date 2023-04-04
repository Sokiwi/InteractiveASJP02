insist_wordlists <- function(input) {
	expect <- 1:10000
	input <- gsub(" ", "", input)
	input <- gsub(",", "", input)
	input <- gsub("-", "", input)
	input <- gsub(";", "", input)
	input <- as.numeric(input)
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
		input <- as.numeric(input)
		w_unexpect <- which(is.na(match(input, expect)))
		cases <- input[w_unexpect]
	}
	return(as.character(input))
}
