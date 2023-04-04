if ( !exists("ethn") ) {
	ethn <<- read.table("e24.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
}
pop_ASJP <- function(x) {
	x1 <- strsplit(x, "")
	if ( length(x1[[1]]) >= 30 ) {
		popstr1 <- x1[[1]][c(19:30)]
		popstr2 <- paste(popstr1, collapse="")
		popstr3 <- trimws(popstr2)
		pop <- suppressWarnings(as.numeric(popstr3))
		if ( is.na(pop) ) {
			return(list(NA, NA))
		}
	} else {
		return(list(NA, NA))
	}
	if ( pop < 0 ) {
		return(list(NA, NA))
	}
	if ( length(x1[[1]]) >= 42 ) {
		isostr1 <- x1[[1]][c(40:42)]
		isostr2 <- paste(isostr1, collapse="")
		iso <- trimws(isostr2)
		if ( is.na(iso)==TRUE ) {
			return(list(NA, NA))
		}
		if ( nchar(iso)==0 ) {
			return(list(NA, NA))
		}
	} else {
		return(list(NA, NA))
	}
	if ( nchar(iso) == 3 | nchar(iso) == 2 ) {
		w <- which(ethn$ISO.639.3==iso)
		if ( length(w) > 0 ) {
			pop_info <- as.numeric(trimws(ethn$Population.Numeric[w]))
			if ( is.na(pop_info) ) {
				return(list(NA, NA))
			} else if ( pop_info==pop ) {
				return(list("same", "same"))
			} else {
				return(list(pop_info, pop))
			}
		} else {
			return(list(NA, NA))
		}
	} else {
		return(list(NA, NA))
	}
}
