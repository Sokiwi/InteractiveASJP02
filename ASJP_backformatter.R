# backformat(data_all, "holman_out.txt")
# takes a file formatted like listss18_formatted.tab as 
# as read by ASJP.R
# and outputs the database
# in original format 
backformat <- function(dataset, outfile_holman) {
	number_three <- match(unique(dataset$wls_fam), dataset$wls_fam)
	fam_gen <- paste(dataset$wls_fam, dataset$wls_gen, sep="__")
	number_two <- match(unique(fam_gen), fam_gen)
	cat("     2     1     0     1    92    72                       \n", file=outfile_holman)
	cat("(I4,20X,10A1)\n", file=outfile_holman, append=TRUE)
	cat("   1                    I\n", file=outfile_holman, append=TRUE)
	cat("   2                    you\n", file=outfile_holman, append=TRUE)
	cat("   3                    we\n", file=outfile_holman, append=TRUE)
	cat("  11                    one\n", file=outfile_holman, append=TRUE)
	cat("  12                    two\n", file=outfile_holman, append=TRUE)
	cat("  18                    person\n", file=outfile_holman, append=TRUE)
	cat("  19                    fish\n", file=outfile_holman, append=TRUE)
	cat("  21                    dog\n", file=outfile_holman, append=TRUE)
	cat("  22                    louse\n", file=outfile_holman, append=TRUE)
	cat("  23                    tree\n", file=outfile_holman, append=TRUE)
	cat("  25                    leaf\n", file=outfile_holman, append=TRUE)
	cat("  28                    skin\n", file=outfile_holman, append=TRUE)
	cat("  30                    blood\n", file=outfile_holman, append=TRUE)
	cat("  31                    bone\n", file=outfile_holman, append=TRUE)
	cat("  34                    horn\n", file=outfile_holman, append=TRUE)
	cat("  39                    ear\n", file=outfile_holman, append=TRUE)
	cat("  40                    eye\n", file=outfile_holman, append=TRUE)
	cat("  41                    nose\n", file=outfile_holman, append=TRUE)
	cat("  43                    tooth\n", file=outfile_holman, append=TRUE)
	cat("  44                    tongue\n", file=outfile_holman, append=TRUE)
	cat("  47                    knee\n", file=outfile_holman, append=TRUE)
	cat("  48                    hand\n", file=outfile_holman, append=TRUE)
	cat("  51                    breast\n", file=outfile_holman, append=TRUE)
	cat("  53                    liver\n", file=outfile_holman, append=TRUE)
	cat("  54                    drink\n", file=outfile_holman, append=TRUE)
	cat("  57                    see\n", file=outfile_holman, append=TRUE)
	cat("  58                    hear\n", file=outfile_holman, append=TRUE)
	cat("  61                    die\n", file=outfile_holman, append=TRUE)
	cat("  66                    come\n", file=outfile_holman, append=TRUE)
	cat("  72                    sun\n", file=outfile_holman, append=TRUE)
	cat("  74                    star\n", file=outfile_holman, append=TRUE)
	cat("  75                    water\n", file=outfile_holman, append=TRUE)
	cat("  77                    stone\n", file=outfile_holman, append=TRUE)
	cat("  82                    fire\n", file=outfile_holman, append=TRUE)
	cat("  85                    path\n", file=outfile_holman, append=TRUE)
	cat("  86                    mountain\n", file=outfile_holman, append=TRUE)
	cat("  92                    night\n", file=outfile_holman, append=TRUE)
	cat("  95                    full\n", file=outfile_holman, append=TRUE)
	cat("  96                    new\n", file=outfile_holman, append=TRUE)
	cat(" 100                    name\n", file=outfile_holman, append=TRUE)
	cat("                                     \n", file=outfile_holman, append=TRUE)
	cat("p\n", file=outfile_holman, append=TRUE)
	cat("b\n", file=outfile_holman, append=TRUE)
	cat("f\n", file=outfile_holman, append=TRUE)
	cat("v\n", file=outfile_holman, append=TRUE)
	cat("m\n", file=outfile_holman, append=TRUE)
	cat("w             \n", file=outfile_holman, append=TRUE)
	cat("8       \n", file=outfile_holman, append=TRUE)
	cat("t\n", file=outfile_holman, append=TRUE)
	cat("d\n", file=outfile_holman, append=TRUE)
	cat("s\n", file=outfile_holman, append=TRUE)
	cat("z\n", file=outfile_holman, append=TRUE)
	cat("c\n", file=outfile_holman, append=TRUE)
	cat("n\n", file=outfile_holman, append=TRUE)
	cat("r\n", file=outfile_holman, append=TRUE)
	cat("l       \n", file=outfile_holman, append=TRUE)
	cat("S\n", file=outfile_holman, append=TRUE)
	cat("Z       \n", file=outfile_holman, append=TRUE)
	cat("C\n", file=outfile_holman, append=TRUE)
	cat("j\n", file=outfile_holman, append=TRUE)
	cat("T       \n", file=outfile_holman, append=TRUE)
	cat("5\n", file=outfile_holman, append=TRUE)
	cat("y\n", file=outfile_holman, append=TRUE)
	cat("k\n", file=outfile_holman, append=TRUE)
	cat("g\n", file=outfile_holman, append=TRUE)
	cat("x\n", file=outfile_holman, append=TRUE)
	cat("N\n", file=outfile_holman, append=TRUE)
	cat("q                     \n", file=outfile_holman, append=TRUE)
	cat("X\n", file=outfile_holman, append=TRUE)
	cat("h\n", file=outfile_holman, append=TRUE)
	cat("7\n", file=outfile_holman, append=TRUE)
	cat("L\n", file=outfile_holman, append=TRUE)
	cat("4\n", file=outfile_holman, append=TRUE)
	cat("G\n", file=outfile_holman, append=TRUE)
	cat("!       \n", file=outfile_holman, append=TRUE)
	cat("i   \n", file=outfile_holman, append=TRUE)
	cat("e\n", file=outfile_holman, append=TRUE)
	cat("E\n", file=outfile_holman, append=TRUE)
	cat("3\n", file=outfile_holman, append=TRUE)
	cat("a\n", file=outfile_holman, append=TRUE)
	cat("u\n", file=outfile_holman, append=TRUE)
	cat("o\n", file=outfile_holman, append=TRUE)
	cat("       \n", file=outfile_holman, append=TRUE)
	cat("                                 \n", file=outfile_holman, append=TRUE)
	for (i in 1:length(dataset[,1])) {
		wls_fam <- dataset$wls_fam[i]
		if ( is.na(wls_fam) ) {
			wls_fam <- ""
		}
		wls_gen <- dataset$wls_gen[i]
		if ( is.na(wls_gen) ) {
			wls_gen <- ""
		}
		wals_clas <- paste(wls_fam, ".", wls_gen, sep="")
		ethn_clas <- dataset$e[i]
		if ( is.na(ethn_clas) ) {
			ethn_clas <- ""
		}
		glot_clas <- dataset$hh[i]
		if ( is.na(glot_clas) ) {
			glot_clas <- ""
		}
		lat <- dataset$lat[i]
		if ( is.na(lat) ) {
			lat <- ""
		}
		lon <- dataset$lon[i]
		if ( is.na(lon) ) {
			lon <- ""
		}
	#	format latitude and longitude
	#	case of no dot
		if ( length(grep("\\.", lat)) == 0 ) {
			lat <- paste(lat, ".00", sep="")
		}
		if ( length(grep("\\.", lon)) == 0 ) {
			lon <- paste(lon, ".00", sep="")
		}
	#	case of a dot and one digit after it
		lat <- as.character(lat)
		if ( length(grep("\\.", lat)) == 1 ) {
			if ( nchar(strsplit(lat, "\\.")[[1]][2])==1 ) {
				lat <- paste(lat, "0", sep="")
			}
		}
		lon <- as.character(lon)
		if ( length(grep("\\.", lon)) == 1 ) {
			if ( nchar(strsplit(lon, "\\.")[[1]][2])==1 ) {
				lon <- paste(lon, "0", sep="")
			}
		}
	#	add spaces before the coordinates
		length_lat <- nchar(as.character(lat))
		lat <- paste(paste(rep(" ", 8 - length_lat), collapse=""), lat, sep="")
		length_lon <- nchar(as.character(lon))
		lon <- paste(paste(rep(" ", 8 - length_lon), collapse=""), lon, sep="")
		if ( lat=="     .00" ) {
			lat <- "        "
		}
		if ( lon=="     .00" ) {
			lon <- "        "
		}
		pop <- dataset$pop[i]
		if ( is.na(pop) ) {
			pop <- 0
		}
	#	format the population figure
		length_pop <- nchar(as.character(pop))
		pop <- paste(paste(rep(" ", 12 - length_pop), collapse=""), pop, sep="")
		wls <- dataset$wcode[i]
		if ( is.na(wls) ) {
			wls <- "   "
		}
		lgname <- dataset$names[i]
		isocode <- dataset$iso[i]
		if ( is.na(isocode) ) {
			isocode <- "   "
		}
		cat(lgname, file=outfile_holman, append=TRUE)
		cat("{", file=outfile_holman, append=TRUE)
		if ( i %in% number_three ) {
			cat(wals_clas, "|", ethn_clas, "@", glot_clas, "}\n 3", lat, lon, pop, "   ", wls, "   ", isocode, "\n", sep="", file=outfile_holman, append=TRUE)
		} else if ( i %in% number_two) {
			cat(wals_clas, "|", ethn_clas, "@", glot_clas, "}\n 2", lat, lon, pop, "   ", wls, "   ", isocode, "\n", sep="", file=outfile_holman, append=TRUE)
		} else {
			cat(wals_clas, "|", ethn_clas, "@", glot_clas, "}\n 1", lat, lon, pop, "   ", wls, "   ", isocode, "\n", sep="", file=outfile_holman, append=TRUE)
		}
		for (j in 11:length(dataset[1,])) {
			cat(j-10, " ", names(dataset)[j], "\t", dataset[i,j], " //\n", file=outfile_holman, sep="", append=TRUE)
		}
	}
	cat("     \n", file=outfile_holman, sep="", append=TRUE)
}

