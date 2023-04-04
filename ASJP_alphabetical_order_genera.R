alphabetical_order_genera <- function(genera, doculects) {
	gen_sorted <- sort(genera, decreasing=FALSE)
	out_of_order <- which(genera != gen_sorted)
	return(list(doculects[out_of_order], genera[out_of_order]))
}
