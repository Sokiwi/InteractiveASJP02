LDN <- function(A, B, data) {
	# A and B are two ASJP language names
	# read the data file, remove % and trim whitespaces, 
	# but only if it is not already in memory
	# reduce the matrix to the two relevant languages
	whereA <- which(data[,1]==A)
	whereB <- which(data[,1]==B)
	data.red <- data[c(whereA, whereB),2:41]
	# do LDN for individual word pairs in data.red
	LDN <- function(x,y) {
		LDN <- levenshtein_distance(x,y)/max(nchar(x),nchar(y))
		return(LDN)
	}
	# reduce further when something is missing for the purpose
	# of LDN between words referring to the same concept
	missing <- union(which(data.red[1,]=="XXX"),which(data.red[2,]=="XXX"))
	if(length(missing) > 0 & length(missing) < 40) {
		data.red1 <- data.red[,-missing]
	} else if ( length(missing)==0 ) {
		data.red1 <- data.red
	} else {
		cat(A, "and", B, "are non-overlapping,\n")
		cat("so the LDN could not be calculated\n")
		return()
	}
	# do average LDN for all pairs of words referring to the same concept,
	# take into account synonyms
	LDNs <- rep(0,length(data.red1[1,]))
	for (i in 1:length(data.red1[1,])) {
		L1 <- length(grep(",",data.red1[1,i]))
		L2 <- length(grep(",",data.red1[2,i]))
		if ( L1==0 & L2==0 ) {
			LDNs[i] <- LDN(data.red1[1,i],data.red1[2,i])
		} else if ( L1 > 0 & L2==0 ) {
			w1 <- strsplit(data.red1[1,i],",")[[1]][1]
			w2 <- strsplit(data.red1[1,i],",")[[1]][2]
			w3 <- data.red1[2,i]
			LDNs[i] <- mean(c(LDN(w1,w3),LDN(w2,w3)))
		} else if ( L1==0 & L2 > 0 ) {
			w1 <- data.red1[1,i]
			w2 <- strsplit(data.red1[2,i],",")[[1]][1]
			w3 <- strsplit(data.red1[2,i],",")[[1]][2]
			LDNs[i] <- mean(c(LDN(w1,w2),LDN(w1,w3)))
		} else {
			w1 <- strsplit(data.red1[1,i],",")[[1]][1]
			w2 <- strsplit(data.red1[1,i],",")[[1]][2]
			w3 <- strsplit(data.red1[2,i],",")[[1]][1]
			w4 <- strsplit(data.red1[2,i],",")[[1]][2]
			LDNs[i] <- mean(c(LDN(w1,w3),LDN(w1,w4),LDN(w2,w3),LDN(w2,w4)))
		}
	}
	LDN.lg <- mean(LDNs)
	return(LDN.lg)
}
