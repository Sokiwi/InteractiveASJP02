restore <- function() {
	cat("", file="greeting")
	if ( length(which(dir()=="record.txt")) > 0 ) {
		file.remove("record.txt")
	}
	session_no <- as.numeric(scan("session.txt", what="numeric", quiet=TRUE))
	new_number <- session_no + 1
	cat(new_number, file="session.txt")
}
