LDND <- function(A, B, data) {
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
	if ( length(missing)==39 ) {
		LDND_out <- round(LDN.lg,4)
	} else {
		list1 <- data.red[1,][data.red[1,]!="XXX"]
		list2 <- data.red[2,][data.red[2,]!="XXX"]
		names.grid <- expand.grid(names(list1),names(list2))
		diagonal <- which(as.vector(names.grid[,1])==as.vector(names.grid[,2]))
		w <- expand.grid(list1,list2,stringsAsFactors=FALSE)[-diagonal,] # w is grid of word pairs
		L <- length(w[,1])
		gamma <- rep(0,L)
		for (i in 1:L) {
			c1 <- length(grep(",",w[i,1]))
			c2 <- length(grep(",",w[i,2]))
			if ( c1==0 & c2==0 ) {
				gamma[i] <- LDN(w[i,1],w[i,2])
			} else if ( c1 > 0 & c2==0 ) {
				w1 <- strsplit(w[i,1],",")[[1]][1]
				w2 <- strsplit(w[i,1],",")[[1]][2]
				w3 <- w[i,2]
				gamma[i] <- mean(c(LDN(w1,w3),LDN(w2,w3)))
			} else if ( c1==0 & c2 > 0 ) {
				w1 <- w[i,1]
				w2 <- strsplit(w[i,2],",")[[1]][1]
				w3 <- strsplit(w[i,2],",")[[1]][2]
				gamma[i] <- mean(c(LDN(w1,w2),LDN(w1,w3)))
			} else {
				w1 <- strsplit(w[i,1],",")[[1]][1]
				w2 <- strsplit(w[i,1],",")[[1]][2]
				w3 <- strsplit(w[i,2],",")[[1]][1]
				w4 <- strsplit(w[i,2],",")[[1]][2]
				gamma[i] <- mean(c(LDN(w1,w3),LDN(w1,w4),LDN(w2,w3),LDN(w2,w4)))
			}
		}
	}
	LDND_out <- round(LDN.lg/mean(gamma),4)
	return(LDND_out)
}
