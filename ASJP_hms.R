hms <- function(x) {
	output <- ""
	seconds <- floor(x %% 60)
	minutes <- floor((x / 60) %% 60)
	hours <- floor(x / 3600)
	if ( hours > 0 ) {
		output <- paste(hours, "hrs", minutes, "mins")
	}
	if ( hours==0 & minutes > 0 ) {
		output <- paste(minutes, "mins", seconds, "secs")
	}
	if ( hours==0 & minutes==0 & seconds >= 1 ) {
		output <- paste(round(x), "secs")
	}
	if ( x < 1 ) {
		output <- paste(round(x, 5), "secs")
	}
	return(output)
}
