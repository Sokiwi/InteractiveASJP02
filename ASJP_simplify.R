simplify <- function(x) {
	x1 <- tolower(x)
	x2 <- gsub("=\\|", "!", x1)
	x3 <- gsub("&#39;", "", x2)
	x4 <- gsub(" - ", "-", x3)
	x5 <- gsub("ê", "e", x4)
	x6 <- gsub("í", "i", x5)
	x7 <- gsub("é", "e", x6)
	x8 <- gsub("á", "a", x7)
	x9 <- gsub("ú", "u", x8)
	x10 <- gsub("ó", "o", x9)
}
