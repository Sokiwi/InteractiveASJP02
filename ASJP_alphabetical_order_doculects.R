alphabetical_order_doculects <- function(doculects) {
	docs_sorted <- sort(doculects, decreasing=FALSE)
	out_of_order <- which(doculects != docs_sorted)
	return(doculects[out_of_order])
}
