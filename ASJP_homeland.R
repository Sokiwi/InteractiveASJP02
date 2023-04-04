# Based on a script by Hans-J. Bibiko modified by S. Wichmann
# homeland <- function(homelandTXTFile, homelandPDFFile, optCLASS) {
homeland <- function(homelandTXTFile, optCLASS) {
	# SETTINGS (can be modified by the user, but not given as choices)
	# onePDFFile <- TRUE   # all family PDFs in one PDF file TRUE or FALSE
	printRivers <- FALSE    # add rivers to the maps?  TRUE or FALSE
	zeroGeoDistance <- 0  # which geographical distance should be treated as 0
	minimumDistance <- 0.01     # minimum distance if geographical distance is less or equal to zeroGeoDistance
	tempFileName <- "__temp_Homeland_diff_matrix_0815.tab" 

	# load custom functions
	source("ASJP_get_e_fam.R")  # function get_e_fam
	source("ASJP_get_g_fam.R")  # function get_g_fam

	# define the distance function, modified from 
	# the function distance in the argosfilter packages
	distance2 <- function (coordinates)  {
		lat1 <- coordinates[1]
		lat2 <- coordinates[2]
		lon1 <- coordinates[3]
		lon2 <- coordinates[4]
		if (lat1 == lat2 & lon1 == lon2) 
			distance <- 0
		else {
			rlat1 = radian(lat1)
			rlat2 = radian(lat2)
			rlon = radian(lon2 - lon1)
			distance <- 60 * (180/pi) * acos(sin(rlat1) * sin(rlat2) + 
				cos(rlat1) * cos(rlat2) * cos(rlon))
			distance <- distance * 1852/1000
		}
		distance
	}

	# install packages (if not installed) and load them
	if ( ("maps" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("maps", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	if ( ("mapdata" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("mapdata", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	if ( ("sp" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("sp", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	suppressMessages(library(maps))
	suppressMessages(library(mapdata))
	suppressMessages(library(sp))

	# define files to use
	lgMetaFile <- "tmp_holman_in.txt"   # lanuage meta data file
	diffMatrixFile <- "tmp_LDND_outfile.meg"  # file with distances

	# read the raw language meta data file as array of lines
	lgMetaDataRaw  <- readLines(lgMetaFile, n=-1)

	# get the array of indices of relevant language data
	lgIndices <- grep('\\w+\\{.*\\}', lgMetaDataRaw, perl = TRUE)

	# create internal language meta data frame
	# go through found lgIndices to parse the relevant data needed
	lgNames <- gsub('^(.+?)\\{.*', '\\1', lgMetaDataRaw[lgIndices], perl = TRUE)
	# choice was Glottolog
	if ( optCLASS=="1" ) {
		lgFamilies <- as.vector(unlist(lapply(lgMetaDataRaw[lgIndices], get_g_fam)))
	}
	# choice was WALS
	if ( optCLASS=="2" ) {
		lgFamilies <- gsub('^.+?\\{(.*?)\\..*', '\\1', lgMetaDataRaw[lgIndices], perl = TRUE)
	}
	# choice was Ethnologue
	if ( optCLASS=="3" ) {
		lgFamilies <- as.vector(unlist(lapply(lgMetaDataRaw[lgIndices], get_e_fam)))
	}
	# set invalid data lines lgIndices+1 to "" to get rid of no long/lat values
	lgMetaDataRaw[lgIndices+1] <- gsub('^ \\S+ +$', '', lgMetaDataRaw[lgIndices+1], perl = TRUE)
	# init Lat and Long as double numeric, empty (non-existing) values are set to NA
	lgLat <- as.double(gsub('^ \\S+ +(.*?)(\\s.*|$)', '\\1', lgMetaDataRaw[lgIndices+1], perl = TRUE))
	lgLong <- as.double(gsub('^ \\S+ +\\S+ +(.*?)(\\s.*|$)', '\\1', lgMetaDataRaw[lgIndices+1], perl = TRUE))
	# init internal language meta data frame with header: Family, Lat, Long and row names = language names
	lgs <- data.frame(row.names = lgNames, stringsAsFactors = FALSE)
	lgs <- cbind(lgs, Family = lgFamilies, Lat = lgLat, Long = lgLong)
	# remove unneeded variables
	rm(list = c("lgLat", "lgLong", "lgNames", "lgFamilies", "lgMetaDataRaw", "lgIndices"))

	# read the raw difference data file as array of lines
	diffDataRaw  <- readLines(diffMatrixFile, n=-1)

	# get the array of indices of used language names
	lgNameIndices <-  grep('^\\[[^\\]]*?\\] +#', diffDataRaw, perl = TRUE)

	# get array of used language names in nexus file
	usedLgs <- gsub('^\\[[^\\]]*?\\] +#(.*?)(\\{.*|$)', '\\1', diffDataRaw[lgNameIndices], perl = TRUE)

	# check if all used language names are known
	# cat("\nCheck if used languages have needed meta data...\n")
	if(sum(usedLgs %in% rownames(lgs)) != length(usedLgs)) {
		print(usedLgs[!usedLgs %in% rownames(lgs)])
		stop("These language names are unknown in meta file. Please check.")
	}
	# check for NA values in Lat or Long
	if(is.na(sum(colSums(lgs[usedLgs, c("Lat", "Long")], na.rm = FALSE)))) {
		for (lg in usedLgs) {
			if(is.na(lgs[lg, "Lat"]) || is.na(lgs[lg, "Long"])) {
				print(lg)
			}
		}
		stop("These language names have no geographical coordinates. Please check.")
	}

	# how many used languages
	lgcnt <- length(usedLgs)
	# language meta data frame of all used languages
	lgs_d <- lgs[usedLgs, ]
	# remove big lgs data
	rm("lgs")

	# read difference matrix
	# init internal matrix
	mat <- matrix(NA, lgcnt, lgcnt)
	# the program was written before operating with LDNDs not expressed as percentages
	mat <- mat * 10000

	# get the array of indices of the difference matrix
	diffIndices <- grep('^\\[[^\\]]*?\\d\\] +(?!#)', diffDataRaw, perl = TRUE)
	# add first empty data line since it won't be found by grep

	# check if the number of found difference matrix lines minus 1 (due to regexp) is equal to lgcnt
	if(lgcnt != length(diffIndices)+1) {
		stop("Mismatch in number of used languages and number of found lines as difference matrix data. Please check.")
	}

	# extract the difference matrix
	diffDataRawProcessed <- c("0", gsub('^\\[[^\\]]*?\\d\\] +(.*)', '\\1 0', diffDataRaw[diffIndices], perl = TRUE))
	# remove diffDataRaw
	rm("diffDataRaw")

	# write diffDataRawProcessed to temporary file to speed up parsing by using scan()
	cat(diffDataRawProcessed, file = tempFileName, sep="\n")

	# read temporary difference matrix file to mat
	mat[row(mat) <= col(mat)] <- scan(tempFileName, what="numeric", na.strings = "%%", quiet=TRUE)

	# remove temporary file
	unlink(tempFileName)

	# set row and column names
	row.names(mat) <- usedLgs
	colnames(mat) <- usedLgs

	# create a regular matrix out of the difference matrix data
	ling_dist <- as.dist(t(mat))
	lingDistance <- as.matrix(ling_dist)

	# tidy up memory
	rm(list = c("mat", "diffDataRawProcessed", "ling_dist", "lgNameIndices", "diffIndices"))
	gc(verbose = FALSE, reset = TRUE)

	# calculating the geographical distance matrix
	if ( !identical(usedLgs, rownames(lgs_d)) ) {
		used <- lgs_d[match(usedLgs, rownames(lgs_d)), ]
	} else {
		used <- lgs_d
	}
	coor <- cbind(expand.grid(used$Lat, used$Lat,stringsAsFactors=FALSE), expand.grid(used$Long, used$Long,stringsAsFactors=FALSE))
	d2 <- apply(coor, 1, distance2)
	geoDistance <- matrix(data=d2, nrow=lgcnt, ncol=lgcnt, dimnames=list(usedLgs, usedLgs))
	rm(coor); rm(d2)

	# set geographical distance to a value other than less or equal to 0
	if(zeroGeoDistance < 0 || minimumDistance <= 0) {
		stop("Geographical distances may not be set to a value less or equal to 0")
	}
	geoDistance[geoDistance <= zeroGeoDistance] <- minimumDistance

	# calculating the ratio of linguistic distance to geographical distance
	lingByGeoDistance  <- lingDistance / geoDistance

	# tidy up memory
	rm(list = c("lingDistance", "geoDistance"))
	gc(verbose = FALSE, reset = TRUE)

	# load river data
	# if(printRivers) {
	# 	cat("\nloading of river data...\n")
	# 	load("rivers.Rdata")
	# 	load("river_low_m360_0.Rdata")
	# }

	########### function declarations ###

	# return the homeland distance for a specific family
	get_dist_m_for_fam <- function(fam) {
		v <- row.names(lgs_d[lgs_d[, "Family"] == fam, ])
		lingByGeoDistance[v, v]
	}

	# plot homeland map
	show_map_for_lgs_in_dist <- function(dist_m, title = "map", 
		xlim = NULL, ylim = NULL, marg = 1, resolution = 1, width = 10, 
		height = 10, ...) {
	
		if (is.null(xlim)) {
			xlim <- c(min(lgs_d[row.names(dist_m), "Long"]) - 
				marg, max(lgs_d[row.names(dist_m), "Long"]) + 
				marg)
			ylim <- c(min(lgs_d[row.names(dist_m), "Lat"]) - 
				marg, max(lgs_d[row.names(dist_m), "Lat"]) + 
				marg)
		}
	
		# plot map
		map("worldHires", xlim = xlim, ylim = ylim, resolution = resolution, fill = TRUE, col = "#EEEEEE", 
			...)
	
		# plot rivers
		if(printRivers) {
			plot(rivers[[1]], add=T, col='lightblue', lwd=0.5, xlim=xlim, ylim=ylim)
			niv <- sapply(river_low_m360_0, function(x) {lines(x, col='lightblue', lwd=0.5, xlim=xlim, ylim=ylim)})
		}

		# plot title
		text(mean(xlim), ylim[1] + 4, title)
	}

	harmonic_mean <- function(x) {
		X <- as.vector(x)
		if(length(which(X==0)) > 0) {
			X <- X[-1*which(X==0)]
		}
		N <- length(X)
		if(N==0) return(NA)
		hm <- N/sum(1/X)
		return(max(X)-hm)
	}

	my_mean <- function(x) {
		X <- as.vector(x)
		X <- X[-1*which(X==0)]
		return(mean(X))
	}

	# wrapper function to draw the homeland for a family 'fam'
	drawHome <- function(fam, z = 100, ...) {
		# get distances for family
		familyDistances <- get_dist_m_for_fam(fam)
	
		# plot map only if more than 1 language is given
		if (length(familyDistances) > 1) {
			
			# use the mean to distinguish between 'relevant' (colourised) and 'non-relevant' (red) languages
			# 'z' is only used as multifier for distinguishing between 'relevant' and 'non-relevant'
			familyDistancesMeans <- rowMeans(familyDistances) * z
			# 'relevant' languages are languages whose homeland value is higher than a 5th of the mean of all homeland values
			relevantLgs <- familyDistancesMeans[familyDistancesMeans > max(familyDistancesMeans)/5]
	
			# set colour values
			## cols <- topo.colors(as.integer(max(relevantLgs)))
	
			# print homeland info to text file
			homeland <- names(familyDistancesMeans[which.max(familyDistancesMeans)])
			cat(paste(fam, homeland, lgs_d[homeland,"Lat"], lgs_d[homeland,"Long"], sep="\t"), file=homelandTXTFile, append = TRUE)
			cat("\n", file=homelandTXTFile, append = TRUE)
	
			# plot base map
			## show_map_for_lgs_in_dist(familyDistances, title=fam, width=9, height=9, ...)
	
			# plot all languages as 'non-relevant' (red dots)
			# pch := dot symbol; cex := symbol radius
			## points(lgs_d[row.names(familyDistances), "Long"], lgs_d[row.names(familyDistances), "Lat"], pch = 21, cex=0.6, bg='red')
			# plot 'relevant' languages colourised on top of all
			## points(lgs_d[names(relevantLgs), "Long"], lgs_d[names(relevantLgs), "Lat"], pch = 21, bg = cols[as.integer(relevantLgs)], cex=2)
		}
	}

	# main routine to generate the PDF file
	# get all distinct family names
	allfam <- unique(as.vector(lgs_d[, "Family"]))

	# create text file for outputting details
	cat("Homeland Information File\n\nFormat: Family Language Latitude Longitude\n\n", file = homelandTXTFile, append = FALSE)

	# create a PDF file, for each family one new page
	# HERFRA note that the program will crash if the PDF is already open
	# this should be fixed.
	# pdf(file = homelandPDFFile, onefile = onePDFFile)

	# loop through all families
	for (i in 1:length(allfam)) {
		drawHome(allfam[i], marg=10)
	}

	## dev.off()
}
