empty_columns <- function(x) {
	s <- strsplit(x, "")
	cols <- s[[1]][c(1,3,11,19,31,32,33,37,38,39)]
	spaces <- length(grep(" ", cols))
	return(spaces)
}
