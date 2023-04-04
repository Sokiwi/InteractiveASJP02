# filter by
# 0. no filter
# 1. proto-languages
# 2. ancient attested languages
# 3. languages gone extinct between ancient times and around 1700
# 4. languages gone extinct between around 1700 and the present
# 5. creoles, pidgins, and mixed languages (by the classification chosen)
# 6. speech registers (only relevant for Glottolog and WALS)
# 7. fake, artificial, and spurious languages
# 8. longest list only
# s. a segment of the database
# m. mimimum: minimum number of attestations (1 means no minimum)
# FIRST_LAST: only use a segment of the database defined by the first and last doculect in the segment
# 5-7 depend on the classification, the parameter optCLASS
# optCLASS=="1": Glottolog
# optCLASS=="2": WALS
# optCLASS=="3": Ethnologue
filters <- function(names, data_all, minimum, kind, optCLASS, FIRST, LAST) {
	rc <- c(11,12,13,21,22,28,29,31,32,33,35,38,40,41,44,49,50,51,53,54,57,58,61,63,64,67,68,71,76,82,84,85,87,92,95,96,102,105,106,110) # stands for relevant columns
	# eliminate names of doculects that are outside the segment chosen
	exclude <- c()
	if ( length(grep("s", kind)) > 0 & length(names) > 0 ) {
		w1 <- which(data_all$names==FIRST)
		w2 <- which(data_all$names==LAST)
		if ( w1 > 1 & w2 < length(data_all[,1]) - 1 ) {
			not_wanted <- c(data_all$names[1:(w1-1)],data_all$names[(w2+1):length(data_all$names)])
		}
		if ( w1 > 1 & w2 == length(data_all[,1]) ) {
			not_wanted <- data_all$names[1:(w1-1)]
		}
		if ( w1 == 1 & w2 < length(data_all[,1]) - 1 ) {
			not_wanted <- data_all$names[(w2+1):length(data_all$names)]
		}
		if ( w1 == 1 & w2 == length(data_all[,1]) ) {
			not_wanted <- c()
		}
		names_redux <- names[match(not_wanted, names)]
		names_redux <- names_redux[!is.na(names_redux)]
		if ( length(names_redux) > 0 & length(match(names_redux, names)) > 0 ) {
			names <- names[-match(names_redux, names)]
		}
	}
	# exclude doculects with a minimum of attestations
	if ( as.numeric(minimum) > 1 & length(names) > 0 ) {
		exclude <- c()
		for (i in 1:length(names)) {
			w <- which(data_all[,1]==names[i])
			attestations <- 40 - length(which(data_all[w,rc]=="XXX"))
			if ( attestations < as.numeric(minimum) ) {
				exclude <- c(exclude, names[i])
			}
		}
		if ( length(exclude) > 0 ) {
			names <- names[-match(exclude, names)]	
		}
	}
	# exclude proto-languages
	if ( length(grep("1", kind)) > 0 & length(names) > 0 ) {
		exclude <- c()
		for (i in 1:length(names)) {
			if ( length(grep("^PROTO_", names[i])) > 0 ) {
				exclude <- c(exclude, names[i])
			}
		}
		if ( length(exclude) > 0 ) {
			names <- names[-match(exclude, names)]	
		}
	}
	# exclude ancient attested languages
	if ( length(grep("2", kind)) > 0 & length(names) > 0 ) {
		exclude <- c()
		for (i in 1:length(names)) {
			w <- which(data_all[,1]==names[i])
			if ( data_all$pop[w]==-2 ) {
				exclude <- c(exclude, names[i])
			}
		}
		if ( length(exclude) > 0 ) {
			names <- names[-match(exclude, names)]	
		}
	}
	# exclude languages extinct between ancient times and about 1700
	if ( length(grep("3", kind)) > 0 & length(names) > 0 ) {
		exclude <- c()
		for (i in 1:length(names)) {
			w <- which(data_all[,1]==names[i])
			if ( data_all$pop[w] %in% c(-3:-1700) ) {
				exclude <- c(exclude, names[i])
			}
		}
		if ( length(exclude) > 0 ) {
			names <- names[-match(exclude, names)]	
		}
	}
	# exclude languages after around 1700
	if ( length(grep("4", kind)) > 0 & length(names) > 0 ) {
		exclude <- c()
		for (i in 1:length(names)) {
			w <- which(data_all[,1]==names[i])
			if ( data_all$pop[w]==-1 | data_all$pop[w] < -1700 ) {
				exclude <- c(exclude, names[i])
			}
		}
		if ( length(exclude) > 0 ) {
			names <- names[-match(exclude, names)]	
		}
	}
	# exclude creoles, pidgins, and mixed languages
	if ( length(grep("5", kind)) > 0 & length(names) > 0 ) {
		# classification is Glottolog
		if ( optCLASS=="1" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("MixedLanguage", data_all$hh[w])) > 0 | length(grep("Pidgin", data_all$hh[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
		# classification is WALS
		if ( optCLASS=="2" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("CREOLES_AND_PIDGINS", data_all$wls_gen[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
		# classification is Ethnologue
		if ( optCLASS=="3" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("Pidgin", data_all$e[w])) > 0 | (length(grep("Creole", data_all$e[w])) > 0 | length(grep("Mixedlanguage", data_all$e[w])) > 0) ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
	}
	# exclude speech registers
	if ( length(grep("6", kind)) > 0 & length(names) > 0 ) {
		# classification is Glottolog
		if ( optCLASS=="1" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("SpeechRegister", data_all$hh[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
		# classification is WALS
		if ( optCLASS=="2" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("SPEECH_REGISTER", data_all$wls_gen[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
	}
	# exclude fake, artificial, and spurious languages
	if ( length(grep("7", kind)) > 0 & length(names) > 0 ) {
		# classification is Glottolog
		if ( optCLASS=="1" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("Spurious", data_all$hh[w])) > 0 | length(grep("ArtificialLanguage", data_all$hh[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
		# classification is WALS
		if ( optCLASS=="2" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("ARTIFICIAL", data_all$wls_gen[w])) > 0 | length(grep("^FAKE$", data_all$wls_gen[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
		# classification is Ethnologue
		if ( optCLASS=="3" ) {
			exclude <- c()
			for (i in 1:length(names)) {
				w <- which(data_all[,1]==names[i])
				if ( length(grep("Constructedlanguage", data_all$e[w])) > 0 ) {
					exclude <- c(exclude, names[i])
				}
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
	}
	# only use the longest list if there are multiple doculects for the same ISO-code
	if ( length(grep("8", kind)) > 0 & length(names) > 1 ) {
		exclude <- c()
		w <- match(names, data_all$names)
		dar <- data_all[w,] # stands for data_all_redux
		w_nas <- which(is.na(dar$iso))
		if ( length(w_nas) > 0 ) {
			dar <- dar[-w_nas,]
		}
		if ( length(dar[,1]) > 0 ) {
			si <- names(which(table(dar$iso)==1)) # stands for singleton ISO-codes
			w_si <- match(si, dar$iso)
			dar <- dar[-w_si,]
		}
		if ( length(dar[,1]) > 0 ) {
			mi <- unique(dar$iso) # stands for multiply occurring ISO-codes
			# pick relevant colums plus the first one with the name
			rcn <- c(1,rc)
			for (i in 1:length(mi)) {
				w <- which(dar$iso==mi[i])
				segm <- dar[w,rcn]
				att <- rep(0, length(segm[,1]))
				segm <- cbind(segm, att)
				for (j in 1:length(segm[,1])) {
				segm[j,42] <- length(which(segm[j,2:41]=="XXX"))
				}
				segm <- segm[order(segm[,42]),]
				least_attested <- segm[2:length(segm[,1]),1]
				exclude <- c(exclude, least_attested)
			}
			if ( length(exclude) > 0 ) {
				names <- names[-match(exclude, names)]	
			}
		}
	}
	return(names)
}
