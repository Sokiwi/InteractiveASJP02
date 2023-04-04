LDN_time <- function(N, data_words) {
	source("ASJP_LDN.R")
	source("ASJP_hms.R")
	a <- Sys.time()
	silence <- LDN("ENGLISH", "DANISH", data_words)
	b <- Sys.time()
	proctime_one <- as.vector(b - a)
	proctime_all <- proctime_one*(N * (N - 1))/2
	return(hms(proctime_all))
}
