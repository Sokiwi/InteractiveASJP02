fix_yn <- function(x) {
	if ( tolower(x)=="y" | tolower(x)=="yes" ) {
		x <- "1"
	}
	if ( tolower(x)=="n" | tolower(x)=="no" ) {
		x <- "2"
	}
	return(x)
}

