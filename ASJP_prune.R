# get the lists that are 20 or more missing items
# put them in a list called list_s
# and make a vector of their names called lists_2
prune <- function(names, data_all) {
#	source("ASJP_zero_overlap.R")
#	source("ASJP_insist.R")  # function zero_overlap
#	source("ASJP_regret.R")  # function zero_overlap
#	source("ASJP_fix_yn.R")  # function zero_overlap
	count <- 0
	names_s <- c()
	lengths_s <- c()
	lists_s <- list()
	for (i in 1:length(names)) {
		currlist <- as.vector(unlist(data_all[which(data_all[,1]==names[i]),c(1,11,12,13,21,22,28,29,31,32,33,35,38,40,41,44,49,50,51,53,54,57,58,61,63,64,67,68,71,76,82,84,85,87,92,95,96,102,105,106,110)]))
		L <- length(which(currlist=="XXX"))
		if ( L >= 20 ) {
			count <- count + 1
			names_s[count] <- names[i]
			lengths_s[count] <- L
			lists_s[[count]] <- currlist
		}
	}
	if ( length(names_s)==0 ) {
		return(NULL)
	} 
	if ( length(names_s) > 1 ) {
		cat("\n")
		print(names_s)
		cat("\nThe above list of doculects in your selection\n")
		cat("have 20 or more missing words, which could mean non-overlap\n")
		cat("of attested words for some doculect pairs.\n")
		cat("\nDo you want me to prune your selection if needed to avoid\n")
		cat("a program crash when computing linguistic distances?\n")
		cat("\n   1. Yes.\n   2. No.\n\nType the number and press ENTER ")
		optPURGE <- readline()
		optPURGE <- fix_yn(optPURGE); optPURGE <- insist(optPURGE, "12x"); regret(optPURGE)
		if ( optPURGE=="1" ) {
			lengthdata <- cbind(names_s, lengths_s)
			lengthdata <- lengthdata[order(lengths_s, decreasing = TRUE), ]
			get_rid_of <- c()
			count_culprits <- 0
			for (i in 1:length(lengthdata[,1])) {
				check <- zero_overlap(lists_s)
				if ( check=="no_overlap" ) {
					count_culprits <- count_culprits + 1
					get_rid_of[count_culprits] <- lengthdata[i,1]
					w_culprit <- which(names_s==lengthdata[i,1])
					lists_s[[w_culprit]] <- NA
				}
			}
			if ( length(get_rid_of)==0 ) {
				cat("\nIt was not necessary to prune the selection,\n")
				cat("there is overlapping attestations for all doculect pairs.\n")
				return(NULL)
			} else {
				return(get_rid_of)
			}
		} else {
			return(NULL)
		}
	}
}
