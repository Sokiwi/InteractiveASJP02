trASJP <- function(fn, no_lists) {
	if ( ("readxl" %in% installed.packages())==FALSE ) {
		install.packages("readxl", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
	}
	suppressMessages(library(readxl))
	# install_xlsx()
	# suppressMessages(library(xlsx))
	# choices: 1. one file per doculect, Holman-style, tab-delimited
	cat("Choose among the following options:\n   1. Output one file per doculect\n      (file names will carry the doculect names).\n   2. Concatenate the output in a file formatted for\n      this program and for Holman's software.\n      (recommended if you continue to work with the file in this program)\n   3. Concatenate the output in a tab-delimited format.\n\n")
	cat("Type one of the numbers and press ENTER ")
	optLISTS <- readline()
	optLISTS <- insist(optLISTS, "123x"); regret(optLISTS)
	if ( optLISTS=="2" ) {
		cat("\n\nEither type a file name or type the letter D to use the default\ncalled my_data_holman.txt\nand press ENTER ")
		optFN <- readline()
		if ( tolower(optFN)=="d" ) {
			outfile <- "my_data_holman.txt"
		} else {
			optFN <- insist_overwrite_file(optFN); regret(optFN)
			outfile <- optFN
		}
	}
	if ( optLISTS=="3" ) {
		source("ASJP_parser.R")
		cat("\n\nEither type a file name or\ntype the letter D to use the default\ncalled my_data.tab\nand press ENTER ")
		optFN <- readline()
		if ( tolower(optFN)=="d" ) {
			outfile <- "my_data_holman.txt"
			outfile_concat <- "my_data.tab"
		} else {
			outfile <- "my_data_holman.txt"
			optFN <- insist_overwrite_file(optFN); regret(optFN)
			outfile_concat <- optFN
		}
	}
	cat("\n\nThe program comes with the files IPA.xlsx and ASJPcode.xlsx.\n")
	cat("These provide instructions for transcription.\n")
	cat("You can\n")
	cat("   1. use those files\n")
	cat("   2. use symbol matching files of your own device\n")
	cat("Type one of the numbers and press ENTER ")
	optTRANS <- readline()
	optTRANS <- insist(optTRANS, "12x"); regret(optTRANS)
	if ( optTRANS=="1" )  {
		# IPA <- read.xlsx("IPA.xlsx", 1, encoding="UTF-8", stringsAsFactors=FALSE, header=FALSE)
		suppressMessages(IPA <- read_excel("IPA.xlsx", col_names=FALSE))
		IPA <- unname(unlist(as.data.frame(IPA)))
		for (i in 1:length(IPA)) {
			IPA[i] <- gsub('\\s+', '', IPA[i])
		}
		# ASJPcode <- read.xlsx("ASJPcode.xlsx", 1, encoding="UTF-8", stringsAsFactors=FALSE, header=FALSE)
		# ASJPcode <- ASJPcode[,1]
		suppressMessages(ASJPcode <- read_excel("ASJPcode.xlsx", col_names=FALSE))
		ASJPcode <- unname(unlist(as.data.frame(ASJPcode)))
		for (i in 1:length(ASJPcode)) {
			ASJPcode[i] <- trimws(ASJPcode[i])
		}
	}
	if ( optTRANS=="2" )  {
		cat("\nType of the name of the Excel file with symbols\nto transcribe FROM and press ENTER ")
		optFROM <- readline()
		optFROM <- insist_file(optFROM); regret(optFROM)
		cat("\nType of the name of the Excel file with symbols\nto transcribe TO and press ENTER ")
		optTO <- readline()
		optTO <- insist_file(optTO); regret(optTO)
		# IPA <- read.xlsx(optFROM, 1, encoding="UTF-8", stringsAsFactors=FALSE, header=FALSE)
		# IPA <- IPA[,1]
		# for (i in 1:length(IPA)) {
		#	IPA[i] <- gsub('\\s+', '', IPA[i])
		# }
		suppressMessages(IPA <- read_excel(ptFROM, col_names=FALSE))
		IPA <- unname(unlist(as.data.frame(IPA)))
		for (i in 1:length(IPA)) {
			IPA[i] <- gsub('\\s+', '', IPA[i])
		}
		# ASJPcode <- read.xlsx(optTO, 1, encoding="UTF-8", stringsAsFactors=FALSE, header=FALSE)
		# ASJPcode <- ASJPcode[,1]
		# for (i in 1:length(ASJPcode)) {
		#	ASJPcode[i] <- trimws(ASJPcode[i])
		# }
		suppressMessages(ASJPcode <- read_excel(optTO, col_names=FALSE))
		ASJPcode <- unname(unlist(as.data.frame(ASJPcode)))
		for (i in 1:length(ASJPcode)) {
			ASJPcode[i] <- trimws(ASJPcode[i])
		}
	}
	if ( !exists("asjp") ) {
		asjp <- data_all
	}
	if ( !exists("ethn") ) {
		ethn <<- read.table("e24.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
	}
	if ( !exists("glot") ) {
		glot <<- read.table("glottolog-4-5.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
	}
	if ( !exists("locs") ) {
		locs <<- read.table("e_locs.txt", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="")
	}
	if ( !exists("wals") ) {
		wals <<- read.table("wals_iso_mappings.txt", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="")
	}
	if ( optLISTS=="2" | optLISTS=="3") {
		cat("     2     1     0     1    92    72                       \n", file=outfile)
		cat("(I4,20X,10A1)\n", file=outfile, append=TRUE)
		cat("   1                    I\n", file=outfile, append=TRUE)
		cat("   2                    you\n", file=outfile, append=TRUE)
		cat("   3                    we\n", file=outfile, append=TRUE)
		cat("  11                    one\n", file=outfile, append=TRUE)
		cat("  12                    two\n", file=outfile, append=TRUE)
		cat("  18                    person\n", file=outfile, append=TRUE)
		cat("  19                    fish\n", file=outfile, append=TRUE)
		cat("  21                    dog\n", file=outfile, append=TRUE)
		cat("  22                    louse\n", file=outfile, append=TRUE)
		cat("  23                    tree\n", file=outfile, append=TRUE)
		cat("  25                    leaf\n", file=outfile, append=TRUE)
		cat("  28                    skin\n", file=outfile, append=TRUE)
		cat("  30                    blood\n", file=outfile, append=TRUE)
		cat("  31                    bone\n", file=outfile, append=TRUE)
		cat("  34                    horn\n", file=outfile, append=TRUE)
		cat("  39                    ear\n", file=outfile, append=TRUE)
		cat("  40                    eye\n", file=outfile, append=TRUE)
		cat("  41                    nose\n", file=outfile, append=TRUE)
		cat("  43                    tooth\n", file=outfile, append=TRUE)
		cat("  44                    tongue\n", file=outfile, append=TRUE)
		cat("  47                    knee\n", file=outfile, append=TRUE)
		cat("  48                    hand\n", file=outfile, append=TRUE)
		cat("  51                    breast\n", file=outfile, append=TRUE)
		cat("  53                    liver\n", file=outfile, append=TRUE)
		cat("  54                    drink\n", file=outfile, append=TRUE)
		cat("  57                    see\n", file=outfile, append=TRUE)
		cat("  58                    hear\n", file=outfile, append=TRUE)
		cat("  61                    die\n", file=outfile, append=TRUE)
		cat("  66                    come\n", file=outfile, append=TRUE)
		cat("  72                    sun\n", file=outfile, append=TRUE)
		cat("  74                    star\n", file=outfile, append=TRUE)
		cat("  75                    water\n", file=outfile, append=TRUE)
		cat("  77                    stone\n", file=outfile, append=TRUE)
		cat("  82                    fire\n", file=outfile, append=TRUE)
		cat("  85                    path\n", file=outfile, append=TRUE)
		cat("  86                    mountain\n", file=outfile, append=TRUE)
		cat("  92                    night\n", file=outfile, append=TRUE)
		cat("  95                    full\n", file=outfile, append=TRUE)
		cat("  96                    new\n", file=outfile, append=TRUE)
		cat(" 100                    name\n", file=outfile, append=TRUE)
		cat("                                     \n", file=outfile, append=TRUE)
		cat("p\n", file=outfile, append=TRUE)
		cat("b\n", file=outfile, append=TRUE)
		cat("f\n", file=outfile, append=TRUE)
		cat("v\n", file=outfile, append=TRUE)
		cat("m\n", file=outfile, append=TRUE)
		cat("w             \n", file=outfile, append=TRUE)
		cat("8       \n", file=outfile, append=TRUE)
		cat("t\n", file=outfile, append=TRUE)
		cat("d\n", file=outfile, append=TRUE)
		cat("s\n", file=outfile, append=TRUE)
		cat("z\n", file=outfile, append=TRUE)
		cat("c\n", file=outfile, append=TRUE)
		cat("n\n", file=outfile, append=TRUE)
		cat("r\n", file=outfile, append=TRUE)
		cat("l       \n", file=outfile, append=TRUE)
		cat("S\n", file=outfile, append=TRUE)
		cat("Z       \n", file=outfile, append=TRUE)
		cat("C\n", file=outfile, append=TRUE)
		cat("j\n", file=outfile, append=TRUE)
		cat("T       \n", file=outfile, append=TRUE)
		cat("5\n", file=outfile, append=TRUE)
		cat("y\n", file=outfile, append=TRUE)
		cat("k\n", file=outfile, append=TRUE)
		cat("g\n", file=outfile, append=TRUE)
		cat("x\n", file=outfile, append=TRUE)
		cat("N\n", file=outfile, append=TRUE)
		cat("q                     \n", file=outfile, append=TRUE)
		cat("X\n", file=outfile, append=TRUE)
		cat("h\n", file=outfile, append=TRUE)
		cat("7\n", file=outfile, append=TRUE)
		cat("L\n", file=outfile, append=TRUE)
		cat("4\n", file=outfile, append=TRUE)
		cat("G\n", file=outfile, append=TRUE)
		cat("!       \n", file=outfile, append=TRUE)
		cat("i   \n", file=outfile, append=TRUE)
		cat("e\n", file=outfile, append=TRUE)
		cat("E\n", file=outfile, append=TRUE)
		cat("3\n", file=outfile, append=TRUE)
		cat("a\n", file=outfile, append=TRUE)
		cat("u\n", file=outfile, append=TRUE)
		cat("o\n", file=outfile, append=TRUE)
		cat("       \n", file=outfile, append=TRUE)
		cat("                                 \n", file=outfile, append=TRUE)
	}
	# x <- read_xlsx(fn, 1, encoding="UTF-8", stringsAsFactors=FALSE, header=FALSE)
	# x <- as.data.frame(apply(x,2,trimws), stringsAsFactors=FALSE)
	suppressMessages(x <- read_excel(fn, col_names=FALSE))
	# x <- unname(unlist(as.data.frame(x)))
	x <- apply(x,2,trimws)
	for (i in 2:(as.numeric(no_lists)+1)) {
		lgname <- x[1,i]
		isocode <- x[2,i]
		wls_fam <- x[3,i]
		wls_gen <- x[4,i]
		lat <- x[5,i]
		lon <- x[6,i]
		if ( (is.na(wls_fam) | is.na(wls_gen)) | (wls_fam=="" | wls_gen=="") ) {
			# identify WALS family and genus by first finding the glottolog family
			if ( !is.na(isocode) & isocode!="" ) {
				# try first to see if there is language with the same iso-code and then lift metadata
				where_iso_glot <- which(glot$ISO639.3==isocode)[1]
				where_iso_asjp <- which(asjp$iso==isocode)[1]
				# look for a language with the same iso-code and steal the WALS classification
				if ( !is.na(where_iso_asjp) ) {
					wals_clas <- paste(asjp$wls_fam[where_iso_asjp], ".", asjp$wls_gen[where_iso_asjp], sep="")
				}
			}
			# look for a closely related language and steal the WALS classification
			if ( is.na(where_iso_asjp) & !is.na(where_iso_glot) ) {
				search <- paste(strsplit(glot[where_iso_glot,2], ",")[[1]][1:4], collapse=",")
				where_glot_clas <- grep(search, asjp$hh)[1]
				if ( length(where_glot_clas) > 0 ) {
					wals_clas <- paste(asjp$wls_fam[where_glot_clas], ".", asjp$wls_gen[where_glot_clas], sep="")
				}
			}
			if ( length(which(ls()=="wals_clas")) == 0 ) {
				cat("WALS classification for", lgname, "not found, it will be written as FAMILY.GENUS\n")
				wals_clas <- "FAMILY.GENUS"
			}
		} else {
			wals_clas <- paste(wls_fam, ".", wls_gen, sep="")
		}
#		get the Ethnologue classification
		if ( !is.na(isocode) & isocode!="" ) {
			where_ethn_clas <- which(ethn$ISO.639.3==isocode)
			if ( length(where_ethn_clas) > 0 ) {
				ethn_clas <- gsub(" ", "", ethn$Classification[where_ethn_clas])
			} else {
				ethn_clas <- "ETHNOLOGUE"
				cat("Ethnologue classification for", lgname, "not found, it will be written as ETHNOLOGUE\n")
			}
		} else {
			cat("Because of missing ISO-code the Ethnologue classification for", lgname, "is not found, it will be written as ETHNOLOGUE\n")
		}
#		get the Glottolog classification
		if ( !is.na(isocode) & isocode!="" ) {
			where_glot_clas <- which(glot$ISO639.3==isocode)
			if ( length(where_glot_clas) > 0 ) {
				glot_clas <- glot[where_glot_clas,2]
			} else {
				glot_clas <- "GLOTTOLOG"
				cat("Glottolog classification for", lgname, "not found, it will be written as GLOTTOLOG\n")
			}
		} else {
			cat("Because of missing ISO-code the Ethnologue classification for", lgname, "is not found, it will be written as ETHNOLOGUE\n")
		}
#		get latitude and longitude
#		if not supplied they will be searched for in Glottolog
		if ( (is.na(lat) | is.na(lon)) | (lat=="" | lon=="") ) {
			if ( !is.na(isocode) & isocode!="" ) {
				where_loc <- which(locs$iso_code==isocode)
				if ( length(where_loc) > 0 ) {
					lat <- locs$lat[where_loc]
					lon <- locs$lon[where_loc]
				} else {
					lat <- "999.99"
					lon <- "999.99"
					cat("Coordinates for", lgname, "not found, they will be written as 999.99 and 999.99\n")
				}
			} else {
				cat("Because of missing ISO-code coordinates for", lgname, "are not found, they will be written as 999.99 and 999.99\n")
			}
		}
#		format latitude and longitude
#		case of no dot
		if ( length(grep("\\.", lat)) == 0 ) {
			lat <- paste(lat, ".00", sep="")
		}
		if ( length(grep("\\.", lon)) == 0 ) {
			lon <- paste(lon, ".00", sep="")
		}
#		case of a dot and one digit after it
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
#		case of more than two digits after the dot
		lat <- as.character(lat)
		if ( length(grep("\\.", lat)) == 1 ) {
			if ( nchar(strsplit(lat, "\\.")[[1]][2]) > 2 ) {
				pre <- nchar(strsplit(lat, "\\.")[[1]][1])
				lat <- paste(strsplit(lat, "")[[1]][1:(pre+3)], collapse="")
			}
		}
		lon <- as.character(lon)
		if ( length(grep("\\.", lon)) == 1 ) {
			if ( nchar(strsplit(lon, "\\.")[[1]][2]) > 2 ) {
				pre <- nchar(strsplit(lon, "\\.")[[1]][1])
				lon <- paste(strsplit(lon, "")[[1]][1:(pre+3)], collapse="")
			}
		}
		# add spaces before the coordinates
		length_lat <- nchar(as.character(lat))
		lat <- paste(paste(rep(" ", 8 - length_lat), collapse=""), lat, sep="")
		length_lon <- nchar(as.character(lon))
		lon <- paste(paste(rep(" ", 8 - length_lon), collapse=""), lon, sep="")
		if ( !is.na(isocode) & isocode!="" ) {
			# get population figure from Ethnologue
			where_ethn_pop <- which(ethn$ISO.639.3==isocode)
			if ( length(where_ethn_pop) > 0 ) {
				pop <- ethn$Population.Numeric[where_ethn_clas]
				if ( is.na(pop) ) {
					pop <- 0
					cat("Either", lgname, "is extinct or the population is unknown\n")
				}
				if ( pop==0 ) {
					cat("Check whether", lgname, "is extinct or the population is unknown\n")
				}
			} else {
				pop <- 0000
				cat("A population figure for", lgname, "could not found, it will be written as 0000\n")
			}
		} else {
			pop <- 0000
			cat("Because of missing ISO-code a population figure for", lgname, "could not found, it will be written as 0000\n")
		}
#		format the population figure
		length_pop <- nchar(as.character(pop))
		pop <- paste(paste(rep(" ", 12 - length_pop), collapse=""), pop, sep="")
#		get the WALS code
		if ( !is.na(isocode) & isocode!="" ) {
			where_wals_code <- which(wals$iso_code==isocode)[1]
			if ( !is.na(where_wals_code) ) {
				wls <- wals[where_wals_code,1]
			} else {
				wls <- "   "
				cat("A WALS code could not be found for ", lgname, ", \n  it will be written as three spaces\n", sep="")
			}
		} else {
			wls <- "   "
			cat("Because of missing ISO-code a WALS code could not be found for ", lgname, ", \nit will be written as three spaces\n", sep="")
		}
#		check if the name is already used
		lgname <- toupper(lgname)
		is_in_asjp <- which(asjp$names==lgname)
		if ( length(is_in_asjp) > 0 ) {
			cat("The name", lgname, "is already used for a", asjp[is_in_asjp, 2], "language\n")
			cat("If at some point you merge your data with ASJP the name should be changed\n")
		}
		if ( is.na(isocode) | isocode=="" ) {
			isocode <- "   "
			cat("An ISO-code was not supplied for ", lgname, ". It will be written as three spaces\n", sep="")
		}
		if ( optLISTS=="1" ) {
			outfile <- paste("auto ", lgname, ".txt", sep="")
			cat("", file=outfile)
		}
		cat(lgname, file=outfile, append=TRUE)
		cat("{", file=outfile, append=TRUE)
		cat(wals_clas, "|", ethn_clas, "@", glot_clas, "}\n 1", lat, lon, pop, "   ", wls, "   ", isocode, "\n", sep="", file=outfile, append=TRUE)
		prefixes <- c("1 I\t", "2 you\t", "3 we\t", "4 this\t", "5 that\t", "6 who\t", "7 what\t", "8 not\t", "9 all\t", "10 many\t", "11 one\t", "12 two\t", "13 big\t", "14 long\t", "15 small\t", "16 woman\t", "17 man\t", "18 person\t", "19 fish\t", "20 bird\t", "21 dog\t", "22 louse\t", "23 tree\t", "24 seed\t", "25 leaf\t", "26 root\t", "27 bark\t", "28 skin\t", "29 flesh\t", "30 blood\t", "31 bone\t", "32 grease\t", "33 egg\t", "34 horn\t", "35 tail\t", "36 feather\t", "37 hair\t", "38 head\t", "39 ear\t", "40 eye\t", "41 nose\t", "42 mouth\t", "43 tooth\t", "44 tongue\t", "45 claw\t", "46 foot\t", "47 knee\t", "48 hand\t", "49 belly\t", "50 neck\t", "51 breast\t", "52 heart\t", "53 liver\t", "54 drink\t", "55 eat\t", "56 bite\t", "57 see\t", "58 hear\t", "59 know\t", "60 sleep\t", "61 die\t", "62 kill\t", "63 swim\t", "64 fly\t", "65 walk\t", "66 come\t", "67 lie\t", "68 sit\t", "69 stand\t", "70 give\t", "71 say\t", "72 sun\t", "73 moon\t", "74 star\t", "75 water\t", "76 rain\t", "77 stone\t", "78 sand\t", "79 earth\t", "80 cloud\t", "81 smoke\t", "82 fire\t", "83 ash\t", "84 burn\t", "85 path\t", "86 mountain\t", "87 red\t", "88 green\t", "89 yellow\t", "90 white\t", "91 black\t", "92 night\t", "93 hot\t", "94 cold\t", "95 full\t", "96 new\t", "97 good\t", "98 round\t", "99 dry\t", "100 name\t")
		for (j in 7:106) {
			cat(prefixes[j-6], file=outfile, sep="", append=TRUE)
			if ( is.na(x[j,i]) | x[j,i]=="" ) {
				cat("XXX", file=outfile, sep="", append=TRUE)
			} else if ( x[j,i]=="XXX" ) {
				cat("XXX", file=outfile, sep="", append=TRUE)
			} else {
				x1 <- strsplit(x[j,i], "")
				for (k in 1:length(x1[[1]])) {
					if ( x1[[1]][k]=="," ) {
						cat(",", file=outfile, sep="", append=TRUE)
					} else if ( x1[[1]][k]==" " ) {
						cat(" ", file=outfile, sep="", append=TRUE)
					} else {
						w <- which(IPA==x1[[1]][k])
						to <- ASJPcode[w]
						if ( length(to) > 0 ) {
							cat(to, file=outfile, sep="", append=TRUE)
						}
					}
				}
			}
			cat(" //\n", file=outfile, sep="", append=TRUE)
		}
	}
	if ( optLISTS=="2" | optLISTS=="3" ) {
		cat("     \n", file=outfile, append=TRUE)
	}
	if ( optLISTS=="3" ) {
		parseASJP(outfile, outfile_concat)
		invisible(file.remove(outfile))
	}
}

