# Credits to Hans-J. Bibiko for supplying most of the code
# for this function
# x and y are vectors of latitudes and longitudes
simple_map <- function(x, y) {
	if ( ("maptools" %in% installed.packages())==FALSE ) {
		install.packages("maptools", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
	}
	if ( ("rgeos" %in% installed.packages())==FALSE ) {
		install.packages("rgeos", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
	}
	suppressMessages(library(maptools))
	nas <- unique(which(is.na(x)), which(is.na(y)))
	if ( length(nas) > 0 ) {
		x <- x[-nas]; y <- y[-nas]
	}
	if ( length(x)==0 | length(y)==0 ) {
		cat("\nThere are no coordinates to plot\n")
		return
	}
	md <- cbind(as.numeric(x), as.numeric(y))
# get max min long and lat and add a frame of 10% around the points
	lat_range <- range(md[,1])
	lng_range <- range(md[,2])
	lat_extend <- 0.1 * diff(lat_range)
	lng_extend <- 0.1 * diff(lng_range)
	lat_range <- c(lat_range - lat_extend, lat_range + lat_extend)
	lng_range <- c(lng_range - lng_extend, lng_range + lng_extend)
#	for (i in 1:4) {
#		if (lng_range[i] > 180) {lng_range[i] <- 180}
#		if (lng_range[i] < -180) {lng_range[i] <- -180}
#	}
	data(wrld_simpl)
	plot(wrld_simpl, col = 'grey', border = NA, axes = T, xlim=c(min(lng_range), max(lng_range)), ylim=c(min(lat_range), max(lat_range)))
	for (i in 1:length(md[,1])) {
		points(md[i,2],md[i,1],col="red",cex=1,pch=19)
	}
}
