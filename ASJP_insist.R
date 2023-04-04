insist <- function(input, expect) {
	input <- gsub(" ", "", input)
	input <- gsub(",", "", input)
	input <- gsub("-", "", input)
	input <- gsub(";", "", input)
	input_s <- strsplit(input,"")[[1]]
	expect_s <- strsplit(expect,"")[[1]]
	w_unexpect <- which(is.na(match(input_s, expect_s)))
	cases <- paste(unique(input_s[w_unexpect]), collapse=" ")
	while ( length(w_unexpect) > 0 ) {
		cat("\n", cases, "is unexpected input\n")
		cat("\nPlease try again here: ")
		input <- readline()
		input_s <- strsplit(input,"")[[1]]
		input <- gsub(" ", "", input)
		input <- gsub(",", "", input)
		input <- gsub("-", "", input)
		input <- gsub(";", "", input)
		w_unexpect <- which(is.na(match(input_s, expect_s)))
		cases <- paste(unique(input_s[w_unexpect]), collapse=" ")
	}
	return(input)
}

