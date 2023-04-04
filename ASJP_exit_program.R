exit_program <- function(session_no, user) {
	cat("\n\nSee you later, ", user, ".\n\n", sep="")
	cat("", file="greeting")
	cat(session_no, file="session.txt")
	if ( length(which(dir()=="record.txt")) > 0 ) {
		invisible(file.remove("record.txt"))
	}
	cat("1", file="soft_exit")
}

