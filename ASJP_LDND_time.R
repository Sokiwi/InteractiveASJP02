LDND_time <- function(N, data_words) {
	source("ASJP_LDND.R")
	source("ASJP_hms.R")
	a <- Sys.time()
	silence <- LDND("ENGLISH", "DANISH", data_words)
	b <- Sys.time()
	proctime_one <- as.vector(b - a)
	proctime_all <- proctime_one*(N * (N - 1))/2
	return(hms(proctime_all))
}
