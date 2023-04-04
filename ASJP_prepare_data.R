prepare_data <- function(data_file_name="listss18_formatted.tab") {
	# cat("Preparing data...\n")
	data_all <- read.table(file=data_file_name, header=TRUE, sep="\t", quote="", na.strings="", strip.white=TRUE, comment.char="", stringsAsFactors=FALSE, colClasses="character",numerals="no.loss")
	# replace a hyphen in doculect names with _, since the former
	# will upset age calculation and maybe other things
	w_hyphen <- grep("-", data_all$names)
	if ( length(w_hyphen) > 0 ) {
		for (i in 1:length(w_hyphen)) {
			data_all$names[w_hyphen[i]] <- gsub("-", "_", data_all$names[w_hyphen[i]])
		}
	}
	data <- data_all[,c(1,11,12,13,21,22,28,29,31,32,33,35,38,40,41,44,49,50,51,53,54,57,58,61,63,64,67,68,71,76,82,84,85,87,92,95,96,102,105,106,110)]
	# replace NAs with XXX
	XXX <- function(x) {
		if ( is.na(x) ) {
			x <- "XXX"
		}
		return(x)
	}
	data[,2:41] <- apply(data[,2:41], c(1,2), XXX)
	# cat("getting rid of the loanword symbol...\n")
	# get rid of the loanword symbol
	eliminate.loans <- function(x) {
		where.loan <- grep("%", x)
		if(length(where.loan) > 0) {
			for (i in 1:length(where.loan)) {
				x[where.loan[i]] <- gsub("%", "", x[where.loan[i]])
			}
		}
		return(x)
	}
	data[,2:41] <- apply(data[,2:41], c(1,2), eliminate.loans)
	# trim whitespace
	# cat("trimming whitespace...\n")
	trim <- function(x) {
		whites <- which(strsplit(x, "")[[1]]==" ")
		if( length(whites) > 0 ) {
			x <- paste(strsplit(x, " ")[[1]][1:(length(whites)+1)],collapse="")
		} else {
			x <- x
		}
	return(x)
	}
	data <- apply(data, c(1,2), trim)
	# get rid of more than two synonyms
	# cat("getting rid of more than two synonyms...\n")
	synred <- function(x) {
		if( length(strsplit(x,",")[[1]]) > 2 ) {
			x <- paste(strsplit(x,",")[[1]][1:2],collapse=",")
		} else {
			x <- x
		}
	return(x)
	}
	data <- apply(data, c(1,2), synred)
	# get a vector of unique symbols, including strings
	# to be treated as one symbol
	# first a function for splitting words, sw
	sw <- function(x) {
		ws <- strsplit(x,"")[[1]]
		# QUOTES
		wq <- grep("\"",ws)
		if ( length(wq) > 0 ) {
			for (i in 1:length(wq)) {
				ws[wq[i]-1] <- paste(ws[wq[i]-1],ws[wq[i]],sep="")
			}
		ws <- ws[-wq]
		}
		# STARS
		wst <- grep("\\*",ws)
		if ( length(wst) > 0 ) {
			for (i in 1:length(wst)) {
				ws[wst[i]-1] <- paste(ws[wst[i]-1],ws[wst[i]],sep="")
			}
		ws <- ws[-wst]
		}
		# TILDE
		wt <- grep("\\~",ws)
		if ( length(wt) > 0 ) {
			for (i in 1:length(wt)) {
				ws[wt[i]-2] <- paste(ws[wt[i]-2],ws[wt[i]-1],sep="")
			}
		ws <- ws[-c(wt,wt-1)]
		}
		# DOLLAR
		wd <- grep("\\$",ws)
		if ( length(wd) > 0 ) {
			for (i in 1:length(wd)) {
				ws[wd[i]-3] <- paste(ws[wd[i]-3],ws[wd[i]-2],ws[wd[i]-1],sep="")
			}
		ws <- ws[-c(wd,wd-1,wd-2)]
		}
		return(unique(ws))
	}
	l <- lapply(data[,2:41],sw)
	uni <- unique(unlist(l))
	rm(l)
	# the following is a trick to get the comma in position 44
	# this will make the value of the comma stay as a comma after being 
	# transformed to a number in the list of unique symbols and then
	# transformed to unicode
	L_uni <- length(uni)
	wcomma <- which(uni==",")
	if ( length(wcomma) > 0 ) {
		if ( L_uni > 44 ) {
			uni <- uni[-wcomma]
			uni <- c(uni[1:43],",",uni[44:length(uni)])
		} else if ( L_uni <= 44 ) {
			uni <- uni[-wcomma]
			uni <- c(uni[1:43],",")
		}
	}
	# the following is a trick to get the X in position 88
	# this will make the value of the X stay as an X after being 
	# transformed to a number in the list of unique symbols and then
	# transformed to unicode
	# in order not to disturb the position of the comma
	# an occurrence of X before 44 is replaced by the dummy _
	L_uni <- length(uni)
	wX <- which(uni=="X")
	if ( length(wX) > 0 ) {
		if ( L_uni > 87 ) {
			uni[wX] <- "_"
			uni <- c(uni[1:87],"X",uni[88:length(uni)])
		} else if ( L_uni <= 87 ) {
			uni[wX] <- "_"
			uni <- c(uni[1:87],"X")
		}
	}
	assign("uni", uni, envir=.GlobalEnv)
	# translate sound-encoding sequences to numbers
	# then to utf8 and then to words
	# cat("transforming sound representations to single utf8 symbols...\n")
	transf <- function(x) {
		# first split the string into individual symbols
		ws <- strsplit(x,"")[[1]]  # stands for word split
		# now start joining the ones that should be treated as 
		# single symbols
		# find quotes and join them with their preceding symbol
		wq <- grep("\"",ws)  # stands for where quote
		if ( length(wq) > 0 ) {
			for (i in 1:length(wq)) {
				ws[wq[i]-1] <- paste(ws[wq[i]-1],ws[wq[i]],sep="")
			}
			ws <- ws[-wq]
		}
		# find stars and join them with their preceding symbol
		wst <- grep("\\*",ws)  # stands for where star
		if ( length(wst) > 0 ) {
			for (i in 1:length(wst)) {
				ws[wst[i]-1] <- paste(ws[wst[i]-1],ws[wst[i]],sep="")
			}
			ws <- ws[-wst]
		}
		# find tildes and join them with their preceding symbol
		wt <- grep("\\~",ws)  # stands for where tilde
		if ( length(wt) > 0 ) {
			for (i in 1:length(wt)) {
				ws[wt[i]-2] <- paste(ws[wt[i]-2],ws[wt[i]-1],sep="")
			}
			ws <- ws[-c(wt,wt-1)]
		}
		# find dollar signs and join them with their two preceding symbols
		wd <- grep("\\$",ws)  # stands for where dollar
		if ( length(wd) > 0 ) {
			for (i in 1:length(wd)) {
				ws[wd[i]-3] <- paste(ws[wd[i]-3],ws[wd[i]-2],ws[wd[i]-1],sep="")
			}
			ws <- ws[-c(wd,wd-1,wd-2)]
		}
		# for each symbol or symbol combination in the word
		# look up its place in the vector of unique symbols;
		# its place there is a number which is subsequently
		# transformed into one of the more than 1 million unicode symbols
		for (i in 1:length(ws)) {
			ws[i] <- which(uni==ws[i])
		}
		# the last value, that of out, will not be printed but if the 
		# output is assigned to a variable this value will pass on
		out <- intToUtf8(ws)
	}
	data[,2:41] <- apply(data[,2:41], c(1,2), transf)
	return(list(data, data_all))
}
