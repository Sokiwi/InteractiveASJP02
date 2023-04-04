install_xlsx <- function() {
	if ( ("xlsx" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("xlsx", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	if ( ("xlsx" %in% installed.packages())==FALSE ) {
		cat("\nFor transcribing own data the xlsx package is required.\n")
		cat("I just tried to install it but failed.\n")
		cat("It may work next time if you install the latest version of R\n")
		cat("as well as the latest version of Java.\n")
	}
}
