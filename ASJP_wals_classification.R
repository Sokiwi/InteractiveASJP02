if ( !exists("wals") ) {
	wals <<- read.table("wals_iso_mappings.txt", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="")
}
# The following function takes a first and a second line of
# metadata for a doculect as input.
# They are ordered as belonging to each of two
# columens in a cbind of firstlines and secondlines
# It checks whether the WALS classification is correct
# for the given WALS code
class_WALS <- function(x) {
	first <- x[1]; second <- x[2]
	wls_code <- get_wls_code(second)
	if ( !is.na(wls_code) ) {
		wls_fam_abbr <- get_wls_fam(first)
		wls_gen <- get_wls_gen(first)
		w <- which(fam_abbr[,2]==wls_fam_abbr)
		wls_fam_full <- fam_abbr[w,1]
		wals_page <- readLines(paste("https://wals.info/languoid/lect/wals_code_", wls_code, sep=""), encoding="UTF-8")
		wfam <- grep("Family: ", wals_page)[1]
		fline <- strsplit(wals_page[wfam], "title=\"")[[1]][2]
		fam_wals <- strsplit(fline, "\"")[[1]][1]
		wgen <- grep("Genus: ", wals_page)[1]
		gline <- strsplit(wals_page[wgen], "title=\"")[[1]][2]
		gen_wals <- strsplit(gline, "\"")[[1]][1]
		# returns family in ASJP, family in WALS
		# genus in ASJP, genus in WALS, all as is
		return(list(wls_fam_full, fam_wals, wls_gen, gen_wals))
	} else {
		return(NA)
	}
}
