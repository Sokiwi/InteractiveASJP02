count_words <- function(x) {
	return(length(strsplit(x, ",")[[1]]))
}
