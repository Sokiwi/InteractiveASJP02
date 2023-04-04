# Credits to Hans-J. Bibiko for supplying most of the code
# for this function
topo_map <- function(x, y, filename) {
	if ( ("OpenStreetMap" %in% installed.packages())==FALSE ) {
		install.packages("OpenStreetMap", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
	}
	suppressMessages(library(OpenStreetMap))
	if ( ("sp" %in% installed.packages())==FALSE ) {
		install.packages("sp", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
	}
	suppressMessages(library(sp))

	# check for missing values
	x <- as.numeric(x)
	y <- as.numeric(y)
	nas <- unique(which(is.na(x)), which(is.na(y)))
	if ( length(nas) > 0 ) {
		x <- x[-nas]; y <- y[-nas]
	}
	if ( length(x)==0 | length(y)==0 ) {
		cat("\nThere are no coordinates to plot\n")
		return
	}

	# get max min long and lat
	lat_range <- range(x, na.rm=T)
	lng_range <- range(y, na.rm=T)

	# add a frame of 10% around the points
	lat_extend <- 0.1 * diff(lat_range)
	lng_extend <- 0.1 * diff(lng_range)
	lat_range <- c(lat_range - lat_extend, lat_range + lat_extend)
	lng_range <- c(lng_range - lng_extend, lng_range + lng_extend)

	# get openstreetmap data - see ?openmap for "type" to get the list of map styles
	omap <- openmap(c(max(lat_range), min(lng_range)), c(min(lat_range), max(lng_range)), type="bing")

	# get map projection
	omap_proj <- omap$tiles[[1]]$projection

	# create a spatial object of read data
	data_sp <- SpatialPoints(cbind(y,x))
	proj4string(data_sp) <- CRS("+proj=longlat")

	# transform the projection of data read to the one used by openstreet
	data_sp_proj <- spTransform(data_sp, omap_proj)

	# plot
	png(filename)
	plot(omap)
	plot(data_sp_proj, pch=21, bg="red", add=T)
	dev.off()
}

