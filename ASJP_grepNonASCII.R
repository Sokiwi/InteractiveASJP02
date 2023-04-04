grepNonASCII <- function(x) {
	asc <- iconv(x, to="ASCII")
	ind <- is.na(asc) | asc != x
	which(ind)
}
