if ( !exists("coor_info") | !exists("data_all") ) {
	coor_info <- read.table(file="e_locs.txt", header=TRUE, stringsAsFactors=FALSE, na.strings="")
}
if ( ("argosfilter" %in% installed.packages())==FALSE ) {
	install.packages("argosfilter", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
}
suppressMessages(library(argosfilter))

coor_ASJP <- function(x, ci=coor_info) {
	x1 <- strsplit(x, "")
	if ( length(x1[[1]]) >= 18 ) {
		latstr1 <- x1[[1]][c(4:10)]
		latstr2 <- paste(latstr1, collapse="")
		latstr3 <- trimws(latstr2)
		lat <- as.numeric(latstr3)
		lonstr1 <- x1[[1]][c(12:18)]
		lonstr2 <- paste(lonstr1, collapse="")
		lonstr3 <- trimws(lonstr2)
		lon <- as.numeric(lonstr3)
	} else {
		return(list("missing_isocode", NA, NA, NA))
	}
	if ( length(x1[[1]]) == 42 ) {
		isostr1 <- x1[[1]][c(40:42)]
		isostr2 <- paste(isostr1, collapse="")
		iso <- trimws(isostr2)
		if ( nchar(trimws(iso)) < 2 ) {
			iso <- "missing_isocode"
		}
	} else {
		iso <- "missing_isocode"
	}
	if (  nchar(iso) == 3 | nchar(iso) == 2  ) {
		w <- which(coor_info[,1]==iso)
		if ( length(w) > 0 ) {
			lat_info <- coor_info[w,2]
			lon_info <- coor_info[w,3]
			if ( lat_info==lat & lon_info==lon ) {
				return(list(iso, 0, lat, lon))
			} else {
				ds <- distance(lat, as.numeric(lat_info), lon, as.numeric(lon_info))
				return(list(iso, ds, lat, lon))
			}
		} else {
			return(list(iso, "isocode_not_in_coor_file", lat, lon))
		}
	} else {
		return(list("missing_isocode", NA, lat, lon))
	}
}
