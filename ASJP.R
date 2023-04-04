ASJP <- function() {
	source("ASJP_alphabetical_order_doculects.R") # function alphabetical_order_doculects
	source("ASJP_alphabetical_order_genera.R") # function alphabetical_order_genera
	source("ASJP_backformatter.R") # function backformat
	source("ASJP_capitalize.R")  # function capitalize
	source("ASJP_classlevel.R")  # function classlevel
	source("ASJP_coor.R")  # function coor_ASJP
	source("ASJP_count_words.R")  # function count_words
	# source("ASJP_dates_f.R") # function ASJP_dates_f
	# source("ASJP_dates_s.R") # function ASJP_dates_s
	source("ASJP_empty_columns.R")  # function empty_columns
	source("ASJP_exit_program.R") # function exit_program
	source("ASJP_filter.R") # function filters
	source("ASJP_fix_yn.R") # function fix_yn
	source("ASJP_generate_glottolog_file.R") # function gfile
	source("ASJP_get_doculect.R") # function get_doculetc
	source("ASJP_get_fams.R") # function get_fams
	source("ASJP_get_fams_non_unique.R") # function get_fams_non_unique
	source("ASJP_get_subgroups.R")  # function get_subgroups
	source("ASJP_get_e_cls.R")  # function get_e_cls
	source("ASJP_get_e_fam.R")  # function get_e_fam
	source("ASJP_get_g_cls.R")  # function get_g_cls
	source("ASJP_get_g_fam.R")  # function get_g_fam
	# source("ASJP_get_gamma.R")  # function get_gamma
	source("ASJP_get_iso_code.R")  # function get_iso_code
	# source("ASJP_get_LDNs.R")  # function get_LDNs
	source("ASJP_get_name.R")  # function get_name
	source("ASJP_get_population.R")  # function get_population
	source("ASJP_get_synset.R")  # function get_synset
	source("ASJP_get_wls_cls.R")  # function get_wls_cls
	source("ASJP_get_wls_code.R")  # function get_wls_code
	source("ASJP_get_wls_fam.R")  # function get_wls_fam
	source("ASJP_get_wls_gen.R")  # function get_wls_gen
	source("ASJP_grepNonASCII.R")  # function grepNonASCII
	source("ASJP_hms.R")  # function hms
	source("ASJP_homeland.R")  # function homeland
	source("ASJP_insist.R")  # function insist
	source("ASJP_insist_file.R")  # function insist_file
	source("ASJP_insist_minimum.R")  # function insist_minimum
	source("ASJP_insist_nofilter.R")  # function insist_nofilter
 	source("ASJP_insist_overwrite_file.R") #  function insist_overwrite_file
	source("ASJP_insist_suffix.R")  # function insist_minimum
	source("ASJP_insist_twodigits.R")  # function insist_twodigits
	source("ASJP_insist_wordlists.R")  # function insist_wordlists
	# source("ASJP_install_xlsx.R")  # function install_xlsx
	source("ASJP_LDN.R") # function LDN
	source("ASJP_LDN_or_LDND_for_MEGA.R") # function MEGA_format
	source("ASJP_LDN_time.R") # function LDN_or_LDND_time
	source("ASJP_LDND_time.R") # function LDN_or_LDND_time
	source("ASJP_LDND.R") # function LDND
	source("ASJP_mega_to_matrix.R") # function MEGA_to_matrix
	source("ASJP_mega_to_phylip.R") # function MEGA_to_phylip
	source("ASJP_mega_to_simple.R") # function MEGA_to_simple
	source("ASJP_mega_to_splitstree.R") # function MEGA_to_splitstree
	source("ASJP_parser.R") # parseASJP
	source("ASJP_pop.R")  # function pop_ASJP
	source("ASJP_prepare_data.R") # function prepare data
	source("ASJP_prune.R") # function prune
	source("ASJP_regret.R")  # function regret
	source("ASJP_restore.R") # function restore
	source("ASJP_simple_map.R")  # function simple_map
	source("ASJP_simplify.R")  # function simplify
	source("ASJP_simplify_not_tolower.R")  # function simplify_not_tolower
	source("ASJP_substitute_g_classification.R") # function substitute_g_classification
	source("ASJP_substitute_population.R")  # function substitute_population
	source("ASJP_topographical_map.R")  # function topo_map
	source("ASJP_transcriber.R") # function trASJP
	source("ASJP_wals_classification.R")  # function class_WALS
	source("ASJP_zero_overlap.R")  # function zero_overlap	
	if ( ("textTinyR" %in% installed.packages())==FALSE ) {
		suppressMessages(install.packages("textTinyR", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE))
	}
	require(textTinyR)
	# check if the program was exited properly last time
	# if not, run the restore function
	if ( scan("soft_exit", what="character", quiet=TRUE)=="0" ) {
		restore()
		complaint <- TRUE
	} else {
		complaint <- FALSE
	}
	cat("0", file="soft_exit")
	# say hello and check if the data have been prepared
	# if not, prepare them
	if ( length(which(dir()=="greeting")) > 0 ) {
		cat("\n#########################################################\n")
		cat("Exit this program prematurely by pressing ESC\n")
		cat("  or by clicking on the STOP sign in the R console.\n")
		cat("Type x at any prompt to restart the session.\n")
		cat("Choose the log file option for info on how to give credits.\n")
		cat("###########################################################\n")
		session_no <- as.numeric(scan("session.txt", what="numeric", quiet=TRUE)) + 1
		hour1 <- strsplit(as.character(Sys.time()), " ")[[1]][2]
		hour2 <- strsplit(hour1, ":")[[1]][1]
		user <<- as.vector(Sys.info()[6])
		if ( hour2 >= 0 & hour2 < 12 ) {
			cat("\nGood morning", user, "and welcome to our session no.", session_no, "\n")
		} 
		if ( hour2 >= 12 & hour2 < 18 ) {
			cat("\nGood afternoon", user, "and welcome to our session no.", session_no, "\n")
		} 
		if ( hour2 >= 18 & hour2 < 24 ) {
			cat("\nGood evening", user, "and welcome to our session no.", session_no, "\n")
		}
		if ( complaint==TRUE ) {
			cat("\nI guess we parted a bit abruptly last time.\n")
			cat("If this was due to a program crash or unexpected\n")
			cat("user input that I should learn to anticipate\n")
			cat("please report to wichmannsoeren@gmail.com.\n")
		}
		logfile <<- paste("log", session_no, ".txt", sep="")
		cat("\nSome results of this session can be summarized in a file called\n")
		cat("   ", logfile, "\n")
		cat("Would you like that to happen?\n")
		cat("\n   1. Yes.\n   2. No.\n\nType the number and press ENTER ")
		optREC <- readline()
		optREC <- fix_yn(optREC); optREC <- insist(optREC, "12x"); regret(optREC)
		cat(optREC, file="record.txt")
		if ( exists("data_words") & exists("data_all") ) {
			cat("\nThe version of the ASJP database you worked with during.\n")
			cat("the last session is still in memory. Do you want to\n")
			cat("\n   1. Continue using it?\n   2. Choose another one?\n\nType 1 or 2 and press ENTER ")
			optCONT <- readline()
			optCONT <- fix_yn(optCONT); optCONT <- insist(optCONT, "12x"); regret(optCONT)
			if ( optCONT=="2" ) {
				rm(data_words, pos = ".GlobalEnv"); rm(data_all, pos = ".GlobalEnv")
			}
		}
	}
	if ( !exists("data_words") | !exists("data_all") ) {
		cat("\nYou need the ASJP database even if you want to work with your own data.\n")
		cat("\nWhich version of the database would you like to use?\n")
		cat("\n   1. The latest released version (v20) [recommended].\n   2. The next version still under construction (v21).\n   3. One of the earlier versions (v12-v19).\n\nType 1, 2 or 3 and press ENTER ")
		optDB <- readline()
		optDB <- insist(optDB, "123x"); regret(optDB)
		# the .Rdata files were prepared from files and a script already available, like this:
		#   parseASJP("listss21.txt", "listss21_formatted.tab")
		#   listss21_prepared <- prepare_data("listss21_formatted.tab")	
		#   save(listss21_prepared, file="listss21_prepared.Rdata")
		if ( optDB=="1" ) {
			load("listss20_prepared.Rdata")
			prepare_data_out <- listss20_prepared
		}
		if ( optDB=="2" ) {
			load("listss21_prepared.Rdata")
			prepare_data_out <- listss21_prepared
		}
		if ( optDB=="3" ) {
			cat("\nType a two-digit version number from 12 to 18 and press ENTER\n")
			optVERSION <- readline()
			regret(optVERSION); optVERSION <- insist_twodigits(optVERSION)
			version_file <- paste("listss", optVERSION, "_prepared.Rdata", sep="")
			load(version_file)
			if ( optVERSION=="12" ) {
				prepare_data_out <- listss12_prepared
			}
			if ( optVERSION=="13" ) {
				prepare_data_out <- listss13_prepared
			}
			if ( optVERSION=="14" ) {
				prepare_data_out <- listss14_prepared
			}
			if ( optVERSION=="15" ) {
				prepare_data_out <- listss15_prepared
			}
			if ( optVERSION=="16" ) {
				prepare_data_out <- listss16_prepared
			}
			if ( optVERSION=="17" ) {
				prepare_data_out <- listss17_prepared
			}
			if ( optVERSION=="18" ) {
				prepare_data_out <- listss18_prepared
			}
			if ( optVERSION=="19" ) {
				prepare_data_out <- listss19_prepared
			}
		}
		if ( optREC=="1" ) {
			if ( optDB=="1" ) {
				cat("The database used was ASJP version 20\n", file=logfile, append=TRUE)
			}
			if ( optDB=="2" ) {
				cat("The database used was ASJP version 21\n", file=logfile, append=TRUE)
			}
			if ( optDB=="3" ) {
				cat("The database used was ASJP version", version_file, "\n", file=logfile, append=TRUE)
			}
		}
		# The following is needed several times in the code but might not
		# be needed for what the user wants to do
		if ( !exists("fam_abbr") ) {
			fam_abbr <<- read.table("FamilyAbbreviations.txt", stringsAsFactors=FALSE, header=TRUE, encoding="UTF-8")
		}
	}
	if ( !exists("data_words") ) {
		data_words <<- prepare_data_out[[1]]
	}
	if ( !exists("data_all") ) {
		data_all <<- prepare_data_out[[2]]
	}
	if ( exists("prepare_data_out") ) {
		rm("prepare_data_out")
	}
	session_no <- as.numeric(scan("session.txt", what="numeric", quiet=TRUE)) + 1
	optREC <- scan("record.txt", what="character", quiet=TRUE)
	# choice: 1. two doculects, 2. a set of doculects
	cat("\nChoose among the following options:\n   1. Work with two named doculects.\n   2. Work with a set of doculects.\n   3. Enter the editor's corner.\n\nType the number and press ENTER ")
	optAB <- readline()
	optAB <- insist(optAB, "123x"); regret(optAB)
	# choice was to work with a pair of doculects
	if ( optAB=="1" ) {
		cat("\nPlease type the name of the first doculect ")
		A <- trimws(toupper(readline()))
		regret(A)
		wA <- which(data_words[,1]==A)
		if ( length(wA)==0 ) {
			cat("\nI could not find a doculect with such a name\n")
			cands <- grep(tolower(A), tolower(data_words[,1]))
			if ( length(cands > 0) ) {
				cat("But maybe you intend one of the following:\n\n")
				cat(data_words[,1][cands], sep="\n")
				cat("\n")
				cat("If not, you could go to the https://asjp.clld.org/\n")
				cat("and check what's there.\n")
				cat("If the name is still not correct next time I prompt you\n")
				cat("I will restart the session.\n")
			}
			if ( length(cands) == 0 ) {
				cat("And I cannot find any that are obviously similar\n")
				cat("You could go to the https://asjp.clld.org/\n")
				cat("and check what is there.\n")
				cat("If the name still not correct next time I prompt you\n")
				cat("I will restart the session.\n")
			}
			cat("Please again type the name of the first doculect: ")
			A <- trimws(toupper(readline()))
			regret(A)
			wA <- which(data_words[,1]==A)
			if ( length(wA)==0 ) {
				cat("", file="greeting")
				print(ASJP())
			}
		}
		cat("Please type the name of the second doculect ")
		B <- trimws(toupper(readline()))
		regret(B)
		wB <- which(data_words[,1]==B)
		if ( length(wB)==0 ) {
			cat("I could not find a doculect with such a name\n")
			cands <- grep(tolower(B), tolower(data_words[,1]))
			if ( length(cands > 0) ) {
				cat("But maybe you intend one of the following:\n")
				cat(data_words[,1][cands], sep="\n")
				cat("\n")
				cat("If not, you should go to the https://asjp.clld.org/ and check what's there.\n")
				cat("If the name still not correct next time I prompt you\n")
				cat("I will restart the session.\n")
			}
			if ( length(cands) == 0 ) {
				cat("And I cannot find any that are obviously similar\n")
				cat("You should go to the https://asjp.clld.org/ and check what's there.")
				cat("If the name still not correct next time I prompt you\n")
				cat("I will restart the session.\n")
			}
			cat("\nPlease again type the name of the second doculect: ")
			B <- trimws(toupper(readline()))
			regret(B)
			wB <- which(data_words[,1]==B)
			if ( length(wB)==0 ) {
				cat("", file="greeting")
				print(ASJP())
			}
		}
		# choices: 1. LDN, 2. LDND, 3. time separation
		cat("\nAll set. You now have the below options.\nYou can choose more than one putting the numbers\nright after one another.\n\n   1. LDN\n   2. LDND\n   3. time separation (before present)\n\nType the number(s) and press ENTER ")
		optCDE <- readline()
		# the choice was LDN
		optCDE <- insist(optCDE, "123x"); regret(optCDE)
		if ( length(grep("1", optCDE))==1 ) {
			out1 <- LDN(A,B,data_words)
			cat("\nLDN between", A, "and", B, "is", round(out1, 4), "\n")
			if ( optREC=="1" ) {
				cat("LDN between", A, "and", B, "is", round(out1, 4), "\n", file=logfile, append=TRUE)
			}
		}		
		# the choice was LDND
		if ( length(grep("2", optCDE))==1 ) {
			out2 <- LDND(A,B,data_words)
			cat("\nLDND between", A, "and", B, "is", round(out2, 4), "\n")
			if ( optREC=="1" ) {
				cat("LDND between", A, "and", B, "is", round(out2, 4), "\n", file=logfile, append=TRUE)
			}
		}		
		# the choice was time separation
		if ( length(grep("3", optCDE))==1 ) {
			ldnd <- LDND(A,B,data_words)
			if ( ldnd < 1 ) {
				out3 <- round(1000*((log10(((100-ldnd*100)/100))-log10(0.92))/(2*log10(0.72))),0)
				cat("\n", A, " and ", B, " separated around ", out3, " years ago\n", sep="")
				if ( optREC=="1" ) {
					cat(A, " and ", B, " separated around ", out3, " years ago\n", sep="", file=logfile, append=TRUE)
				}
			} else {
				cat("\nThe time separation between", A, "and", B, "\n")
				cat("is not defined because LDND is greater than or equal to 1.\n")
				if ( optREC=="1" ) {
					cat("\nThe time separation between", A, "and", B, "\n")
					cat("is not defined because LDND is greater than or equal to 1.\n", file=logfile, append=TRUE)
				}
			}
		}		
	}
	# choice was to work with a set of doculects either in ASJP or ones's own
	if ( optAB=="2" ) {
		# choices: 1. set of doculects in ASJP, 2: own data
		cat("\n\nA note on terminology: An \"ASJP-style database\"\n")
		cat("is like listss20.txt, which is among your files.\n")
		cat("See https://asjp.clld.org/help for detail.\n")
		cat("\nYou now have the following options:\n\n")
		cat("   1. Work with a set of doculects in the ASJP database\n")
		cat("      or in your own ASJP-style database.\n")
		cat("   2. Transcribe your own dataset to make an ASJP-style database\n")
		cat("      which you can subsequently work with when returning to this prompt.\n\n")
		cat("Type a number and press ENTER ")
		optDATA <- readline()
		optDATA <- insist(optDATA, "12x"); regret(optDATA)
		# choice was a set of doculects in ASJP or ASJP-style database
		if ( optDATA=="1" ) {
			# choices: 1. named list, 2. families, 3. subgroups, 4. entire database, 5. own database
			cat("\n\nHow is your set of doculects to be defined?\n")
			cat("(note that options 2-4 below allow for further filtering\n")
			cat(" by a mix of criteria)\n\n")
			cat("   1. as a list of names in a text file\n")
			cat("      --before choosing 1, put a file in the current directory.\n")
			cat("      It should have the exact names (case sensitive) of the doculects\n")
			cat("      you are interested in, with one name per line\n")
			cat("      (see the example in the supplied file names_example.txt),\n")
			cat("      or else it should be a list of lowercase ISO-codes, one per line.\n")
			cat("      I will recognize whether you are working with doculect names or ISO-codes\n")
			cat("   2. as one or more language families\n")
			cat("   3. as one or more highest-order subgroups of a language family\n")
			cat("   4. use the entire database, applying other filter options\n")
			cat("   5. use your own ASJP-style database\n")
			cat("\n\nType the number and press ENTER ")
			optLGS <- readline()
			optLGS <- insist(optLGS, "12345x"); regret(optLGS)
			# the classification followed must be known for many purposes
			# choices: 1. Glottolog, 2. WALS, 3. Ethnologue
			cat("\nWhich classification do you want to follow?\n   1. Glottolog\n   2. WALS\n   3. Ethnologue\n\nType the number and press ENTER ")
			optCLASS <- readline()
			optCLASS <- insist(optCLASS, "12345x"); regret(optCLASS)
			# option was a named list
			if ( optLGS=="1" ) {
				cat("\n\nFor the file with doculect names, do you want to\n\n")
				cat("    1. use the default filename names.txt?\n")
				cat("    2. choose your own file name?\n\n")
				cat("Type a number and press ENTER ")
				optNAMESFILE <- readline()
				optNAMESFILE <- insist(optNAMESFILE, "12x"); regret(optNAMESFILE)
				if ( length(grep("1", optNAMESFILE)) > 0 ) {
					names_file <- "names.txt"
				}
				if ( length(grep("2", optNAMESFILE)) > 0 ) {
					cat("\ntype the file name and press ENTER ")
					names_file <- readline()
					names_file <- insist_file(names_file); regret(names_file)
				}
				read_names_file <- scan(names_file, what="character", quiet=TRUE)
				if ( unlist(gregexpr("[A-Z]", read_names_file[1]))[1] != -1 ) {
					names <- read_names_file
					exclude <- c()
					count <- 0
					for (i in 1:length(names)) {
						hit <- which(data_all$names==names[i])
						if ( length(hit)==0 ) {
							cat("The name", names[i], "is not in ASJP and will not be used\n")
							count <- count + 1
							exclude[count] <- i
						}
					}
					if ( length(exclude) > 0 ) {
						names <- names[-exclude]
					}
				} else if ( unlist(gregexpr("[A-Z]", read_names_file[1]))[1] == -1 & nchar(read_names_file[1]) < 4 ) {
					names <- c()
					for (i in 1:length(read_names_file)) {
						hits <- which(data_all$iso==read_names_file[i])
						if ( length(hits) > 0 ) {
							names <- c(names, data_all$names[hits])
						} else {
							cat("The code", read_names_file[i], "is not in ASJP and will not be used\n")
						}
					}
				} else {
					cat("Something is wrong, perhaps you have language names\n")
					cat("but forgot that they should be uppercase\n")
				}
			}
			if ( optLGS=="2" | optLGS=="3" ) {
				# choices: 1. a list of language families, 2. no such list
				cat("\nDo you want a list of names of language families currently in ASJP?\n   1. yes.\n   2. no.\n\nType the number and press ENTER ")
				optOPTIONS <- readline()
				optOPTIONS <- fix_yn(optOPTIONS); optOPTIONS <- insist(optOPTIONS, "12x"); regret(optOPTIONS)
				# choice was to get a list of names
				if ( optOPTIONS=="1" ) {
					# choice for getting a list of family names was Glottolog
					if ( optCLASS=="1" ) {
						fams <- get_fams(data_all$hh)
						cat("\n")
						cat("\n  Glottolog family names:\n")
						print(noquote(sort(fams)))
						cat("\n--End of list of Glottolog families--\n")
						cat("\n")
					}
					# choice for getting a list of family names was WALS
					if ( optCLASS=="2" ) {
						fams <- unique(data_all$wls_fam)
						cat("\n")
						fams_sorted <- sort(fams)
						cat("\n  WALS family abbreviations:\n")
						print(noquote(fams_sorted))
						cat("\n")
						cat("\n--End of list of WALS family abbreviations--\n")
						fams_full <- c()
						for (i in 1:length(fams_sorted)) {
							w <- which(fam_abbr[,2]==fams_sorted[i])
							if ( length(w) > 0 ) {
								fams_full[i] <- simplify(fam_abbr[w,1])
							} else {
								fams_full[i] <- NA
							}
						}
						cat("\n  Full WALS family names:\n")
						print(noquote(fams_full))
						cat("\n")
						cat("\n--End of list of full WALS family names--\n\n")
						cat("You can refer to WALS families either\n")
						cat("by the abbreviation or the full name\n")
					}
					# choice for getting a list of family names was Ethnologue
					if ( optCLASS=="3" ) {
						fams <- get_fams(data_all$e)
						cat("\n")
						cat("Ethnologue families:\n")
						print(noquote(sort(fams)))
						cat("\n--End of list of Ethnologue families--\n")
					}
				}
				cat("\nPlease give me one or more family names or abbreviations--one if you\n")
				cat("want to work with (a) subgroup(s) and one or more if you want to work\n")
				cat("with a whole family.\n\nIf giving me more than one family name,\n")
				cat("please separate them with a comma and do not use any spaces\n")
				# choice of family
				cat("\n\nType the family name(s) and press ENTER ")
				optFAMS <- readline()
				regret(optFAMS)
			}
			# the choice was families
			if ( optLGS=="2" ) {  # family
				remove_space <- function(x) gsub(" ", "", x)
				remove_underscore <- function(x) gsub("_", "", x)
				# the choice of classification for families was Glottolog
				if ( optCLASS=="1" ) {
					return_g_family <- function(x) { 
						x1 <- strsplit(x, ",")[[1]][1]
						x2 <- strsplit(x1, "\\}")[[1]][1]
						return(x2)
					}
					w <- c()
					optFAMS_nospace <- paste(strsplit(optFAMS, " ")[[1]], collapse="")
					fams <- strsplit(optFAMS_nospace, ",")[[1]]
					fams <- as.vector(unlist(lapply(fams, simplify)))
					fams <- as.vector(unlist(lapply(fams, remove_space)))
					fams <- as.vector(unlist(lapply(fams, remove_underscore)))
					g_fams_simple <- as.vector(unlist(lapply(data_all$hh, simplify)))
					g_fams_simple <- as.vector(unlist(lapply(g_fams_simple, remove_space)))
					g_fams_simple <- as.vector(unlist(lapply(g_fams_simple, return_g_family)))
					g_fams_simple <- as.vector(unlist(lapply(g_fams_simple, remove_underscore)))
					for (i in 1:length(fams)) {
						w <- c(w, grep(paste("^", fams[i], "$", sep=""), g_fams_simple))
						if ( length(which(g_fams_simple==fams[i]))== 0 ) {
							cat("There does not seem to be a Glottolog family called", trimws(strsplit(optFAMS, ",")[[1]][i]),"\n")
						}
					}
					names <- data_all$names[w]
				}
				# the choice of classification for families was WALS
				if ( optCLASS=="2" ) {
					w <- c()
					optFAMS_nospace <- paste(strsplit(optFAMS, " ")[[1]], collapse="")
					fams <- strsplit(optFAMS_nospace, ",")[[1]]
					fams <- as.vector(unlist(lapply(fams, simplify)))
					full_simple <- as.vector(unlist(lapply(fam_abbr[,1], simplify)))
					full_simple <- as.vector(unlist(lapply(full_simple, remove_space)))
					for (i in 1:length(fams)) {
						w_full <- which(full_simple==fams[i])
						w_abbr <- which(tolower(fam_abbr[,2])==fams[i])
						if ( length(w_full)==1 ) {
							fams[i] <- fam_abbr[w_full,2]
						}
						if ( length(w_full)==1 | length(w_abbr)==1 ) {
							w <- c(w, grep(paste("^", tolower(fams[i]), "$", sep=""), tolower(data_all$wls_fam)))
							names <- data_all$names[w]
						}
						if ( length(w_full)==0 & length(w_abbr)==0 ) {
							cat("There does not seem to be a WALS family called", trimws(strsplit(optFAMS, ",")[[1]][i]),"\n")
						}
					}
				}
				# the choice of classification for families was Ethnologue
				if ( optCLASS=="3" ) {
					return_e_family <- function(x) { 
						x1 <- strsplit(x, ",")[[1]][1]
						x2 <- strsplit(x1, "@")[[1]][1]
						return(x2)
					}
					optFAMS_nospace <- paste(strsplit(optFAMS, " ")[[1]], collapse="")
					fams <- strsplit(optFAMS_nospace, ",")[[1]]
					fams <- as.vector(unlist(lapply(fams, simplify)))
					fams <- as.vector(unlist(lapply(fams, remove_space)))
					fams <- as.vector(unlist(lapply(fams, remove_underscore)))
					e_fams_simple <- as.vector(unlist(lapply(data_all$e, simplify)))
					e_fams_simple <- as.vector(unlist(lapply(e_fams_simple, remove_space)))
					e_fams_simple <- as.vector(unlist(lapply(e_fams_simple, return_e_family)))
					w <- c()
					for (i in 1:length(fams)) {
						w <- c(w, which(e_fams_simple==fams[i]))
						if ( length(which(e_fams_simple==fams[i]))== 0 ) {
							cat("There does not seem to be an Ethnologue family called", trimws(strsplit(optFAMS, ",")[[1]][i]),"\n")
						}
					}
					names <- data_all$names[w]
				}
			}
			# the choice was subgroups
			if ( optLGS=="3" ) {
				remove_space <- function(x) gsub(" ", "", x)
				remove_underscore <- function(x) gsub("_", "", x)
				# the choice of classification for subgroups was Glottolog
				if ( optCLASS=="1" ) {
					fams <- get_fams_non_unique(data_all$hh)
					w <- which(fams==optFAMS)
					subgroups <- get_subgroups(w, data_all$hh)
				}
				# the choice of classification for subgroups was WALS
				if ( optCLASS=="2" ) {
					w_wals_fam <- c()
					optFAMS_nospace <- paste(strsplit(optFAMS, " ")[[1]], collapse="")
					fams <- strsplit(optFAMS_nospace, ",")[[1]]
					fams <- as.vector(unlist(lapply(fams, simplify)))
					full_simple <- as.vector(unlist(lapply(fam_abbr[,1], simplify)))
					full_simple <- as.vector(unlist(lapply(full_simple, remove_space)))
					for (i in 1:length(fams)) {
						w_full <- which(full_simple==fams[i])
						w_abbr <- which(tolower(fam_abbr[,2])==fams[i])
						if ( length(w_full)==1 ) {
							fams[i] <- fam_abbr[w_full,2]
						}
						if ( length(w_full)==1 | length(w_abbr)==1 ) {
							w_wals_fam <- c(w_wals_fam, grep(paste("^", tolower(fams[i]), "$", sep=""), tolower(data_all$wls_fam)))
							names <- data_all$names[w_wals_fam]
							subgroups <- unique(data_all$wls_gen[w_wals_fam])
						}
						if ( length(w_full)==0 & length(w_abbr)==0 ) {
							cat("There does not seem to be a WALS family called", trimws(strsplit(optFAMS, ",")[[1]][i]),"\n")
						}
					}
				}
				# the choice of classification for subgroups was Ethnologue
				if ( optCLASS=="3" ) {
					fams <- get_fams_non_unique(data_all$e)
					w <- which(fams==optFAMS)
					subgroups <- get_subgroups(w, data_all$e)
				}
				cat("The subgroups to choose from are:\n")
				cat(subgroups, sep="\n")
				cat("\n")
				cat("\n--End of list of subgroups of", optFAMS, "to choose from--\n")
				cat("\nNow I need one or more subgroup names. If giving me")
				cat("\nmore than one subgroup name please separate")
				cat("\nwith a comma and no space\n")
				cat("\n\n\nType the subgroup name(s) and press ENTER ")
				optSUBGROUPS <- readline()
				regret(optSUBGROUPS)
				# the choice was Glottolog subgroups
				if ( optCLASS=="1" ) {
					w <- c()
					subs <- strsplit(optSUBGROUPS, ",")[[1]]
					for (i in 1:length(subs)) {
						fam_plus_sub <- paste(optFAMS, ",", subs[i], sep="")
						w <- c(w, grep(paste("^", fam_plus_sub, "(,|$)", sep=""), data_all$hh))
					}
					names <- data_all$names[w]
				}
				# the choice was WALS subgroups
				if ( optCLASS=="2" ) {  
					w <- c()
					subs <- strsplit(optSUBGROUPS, ",")[[1]]
					for (i in 1:length(subs)) {
						w_sub <- which(data_all$wls_gen==subs[i])
						w_int <- intersect(w_wals_fam, w_sub)
						w <- c(w, w_int)
					}
					names <- data_all$names[w]
				}
				# the choice was Ethnologue subgroups
				if ( optCLASS=="3" ) {
					w <- c()
					subs <- strsplit(optSUBGROUPS, ",")[[1]]
					for (i in 1:length(subs)) {
						fam_plus_sub <- paste(optFAMS, ",", subs[i], sep="")
						w <- c(w, grep(paste("^", fam_plus_sub, "(,|$)", sep=""), data_all$e))
					}
					names <- data_all$names[w]						
				}
			}
			# the choice was the entire database
			if ( optLGS=="4" ) {
				names <- data_all$names
			}
			# the choice was families, subgroups or entire database
			if ( (optLGS=="2" | optLGS=="3") | optLGS=="4" ) {
				cat("\nYou now have the option to further filtering by the following criteria\n\n")
				cat("   0. no filter\n")
				cat("   1. exclude proto-languages\n")
				cat("   2. exclude ancient attested languages\n")
				cat("   3. exclude languages gone extinct between ancient times and around 1700\n")
				cat("   4. exclude languages gone extinct between around 1700 and the present\n")
				cat("   5. exclude creoles, pidgins, and mixed languages\n")
				cat("      (as defined by the classification chosen;\n")
				cat("       note here that Glottolog is largely blind to creoles)\n")
				cat("   6. exclude speech registers\n")
				cat("      (only relevant for Glottolog and WALS)\n")
				cat("   7. exclude fake, artificial, and spurious languages\n")
				cat("      (as defined by the classification chosen;\n")
				cat("       note here that Ethnologue is largely blind to artificial languages)\n")
				cat("   8. when several doculects represent the same ISO-code\n")
				cat("      only use the longest word list\n")
				cat("   s. use only a segment of the database defined by the first and last language\n")
				cat("      (note that the database is ordered by world areas, inspect it to see how)\n")
				cat("   m. choose a minimum of attested words, excluding lists that are shorter\n")
				cat("      (the next prompt will ask for the number)\n")
				cat("\n\nType the numbers and letters right next to each other and press ENTER ")
				kind <- readline()
				kind <- insist_nofilter(kind)
				kind <- insist(kind, "012345678smx")
				regret(kind)
				if ( length(grep("s", kind)) > 0 ) {
					cat("\n\nType the name of the first doculect in the segment and press ENTER ")
					FIRST <- toupper(readline())
					regret(FIRST)
					cat("\n\nType the name of the last doculect in the segment and press ENTER ")
					LAST <- toupper(readline())
					regret(LAST)
				}
				if ( length(grep("m", kind)) > 0 ) {
					cat("\n\nType a number in the range 1-40 for the minimum of attested words and press ENTER ")
					minimum <- readline()
					minimum <- insist_minimum(minimum); regret(minimum)
				}
				if ( length(grep("0", kind))==0 ) {
					if ( length(grep("m", kind))==0 ) {
						minimum <- 1
					}
					if ( length(grep("s", kind))==0 ) {
						FIRST <- names[1]
						LAST<- names[length(names)]
					}
					if ( length(names) > 0 ) {
						names <- filters(names, data_all, minimum, kind, optCLASS, FIRST, LAST)
					}
				}
			}
			if ( optLGS!="5" ) {
				if ( length(names) > 0 ) {
					to_prune <- prune(names, data_all)
					if ( length(to_prune) > 0 ) {
						w_to_prune <- match(to_prune, names)
						if ( length(w_to_prune) > 0 ) {
							cat("\nThe following were pruned from your selection:\n")
							print(names[w_to_prune])
							names <- names[-w_to_prune]
						}
					}
					cat("\nThese are the names of the doculects you have chosen to work with:\n")
					print(names)
					cat("\n--End of list of", length(names), "doculects that you have chosen to work with\n")
				} else {
					cat("\nYour selection is empty.\n") 
					cat("Choose to restart at the next prompt.\n")
				}
				cat("\n\nEstimated max processing time for one or more jobs involving...\n")
				cat("...LDN:", LDN_time(length(names), data_words), "\n")
				cat("..LDND:", LDND_time(length(names), data_words), "\n")
				cat("(The latter includes homelands and dates).\n\n")
				cat("\nAre you satisfied with your selection or do you want to restart the session?\n")
				cat("   1. I am satisfied with my selection\n")
				cat("   2. I want to restart the session\n")
				cat("\n\nType the number and press ENTER ")
				optSATISFIED <- readline()
				optSATISFIED <- fix_yn(optSATISFIED); optSATISFIED <- insist(optSATISFIED, "12x"); regret(optSATISFIED)
				if ( optSATISFIED=="2" ) {
				if ( length(which(dir()=="greeting")) > 0 ) {
						invisible(file.remove("greeting"))
					}
					cat("\n")
					print(ASJP())
				}
				if ( optSATISFIED=="1" ) {
					if ( optREC=="1" ) {
						if ( (optLGS=="2" | optLGS=="3") | optLGS=="4" ) {
							cat("\nYour selection criteria and the set of doculects have been registered in", logfile, "\n")
							cat("The doculects to be used in this session were selected according to the following criteria:\n", file=logfile, append=TRUE)
							if ( length(grep("0", kind)) > 0 ) {
								cat("No filter was used\n", file=logfile, append=TRUE)
							}
							if ( length(grep("1", kind)) > 0 ) {
								cat("Proto-languages were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("2", kind)) > 0 ) {
								cat("Ancient attested languages were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("3", kind)) > 0 ) {
								cat("Languages gone extinct between ancient times and around 1700 were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("4", kind)) > 0 ) {
								cat("Languages gone extinct between around 1700 and the present were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("5", kind)) > 0 ) {
								cat("Creoles, pidgins, and mixed languages were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("6", kind)) > 0 ) {
								cat("Speech registers were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("7", kind)) > 0 ) {
								cat("Fake, artificial, and spurious languages were excluded.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("8", kind)) > 0 ) {
								cat("When several doculects represent the same ISO-code only the longest list was used.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("s", kind)) > 0 ) {
								cat("Only a segment of the database defined by", FIRST, "and", LAST, "was used.\n", file=logfile, append=TRUE)
							}
							if ( length(grep("m", kind)) > 0 ) {
								cat("Only word lists having at least", minimum, "attestations were used.\n", file=logfile, append=TRUE)
							}
						}
						cat("The following doculects were used:\n", file=logfile, append=TRUE)
						cat(names, file=logfile, append=TRUE, sep=", ")
						cat("\n", file=logfile, append=TRUE)
					}
				}
			}
			cat("\n\nUsing your selection of doculects or the database you supplied\n")
			cat("there are now the following options:\n\n")
			if ( optLGS!="5" ) {
				cat("    1. Create an input file for fast .exe software by Holman for computing LDND or dates\n")
				cat("       (see Holman 2011c and 2011d at https://asjp.clld.org/software)\n")
				cat("       --processing time may be around 0.3 seconds per doculect,\n")
				cat("       but each LDND computation of Holman's software is some 60\n")
				cat("       times faster than using option 3 below\n")
			}
			cat("    2. Produce a map of the distribution of the doculects.\n")
			cat("    3. Compute LDND or LDN directly in one or more of different output formats\n")
			cat("       to be specified in the next prompt and/or produce a tree.\n")
			cat("    4. Infer the homeland(s) of the language group(s)\n")
			cat("       using the method of Wichmann et al. (2010)\n")
			cat("       (see Diachronica 27(2), 247-276).\n")
			cat("    5. Infer the date(s) of the language group(s)\n")
			cat("       using the method of Holman et al. (2011)\n")
			cat("       (see Current Anthropology 52(6), 841-875)\n")
			cat("\n\nType one or more numbers next to each other and press ENTER ")
			optOUT <- readline()
			optOUT <- insist(optOUT, "12345x"); regret(optOUT)
			# the choice was output for Holman programs asjp62.exe, asjp62e.exe, aspj62x.exe or asjp62c.exe
			if ( length(grep("1", optOUT)) > 0 & optLGS != "5" ) {
				cat("\n\nFor the input file to Holman's software, do you want to\n\n")
				cat("    1. use the default filename holman_in.txt,\n")
				cat("       which will overwrite an existing file with that name?\n")
				cat("    2. choose your own file name?\n\n")
				cat("Type a number and press ENTER ")
				optOUT_holman <- readline()
				optOUT_holman <- insist(optOUT_holman, "12x"); regret(optOUT_holman)
				if ( length(grep("1", optOUT_holman)) > 0 ) {
					outfile_holman <- "holman_in.txt"
				}
				if ( length(grep("2", optOUT_holman)) > 0 ) {
					cat("Type the file name and press ENTER ")
					outfile_holman <- readline()
					outfile_holman <- insist_overwrite_file(outfile_holman); regret(outfile_holman)
				}
			}
			# the choice was output for Holman programs OR homeland, not using own data
			if ( (length(grep("1", optOUT)) > 0 | length(grep("4", optOUT)) > 0) & optLGS != "5" ) {
				m <- match(names, data_all$names)
				database <- data_all[m,]
			}
			# the choice was output for Holman programs AND homeland, not using own data
			# --here the database selection is backformatted and a copy made for homeland function
			if ( (length(grep("1", optOUT)) > 0 & length(grep("4", optOUT)) > 0) & optLGS != "5" ) {
				backformat(database, outfile_holman)
				invisible(file.copy(outfile_holman, "tmp_holman_in.txt", overwrite=TRUE))
			}
			# the choice was NOT output for Holman programs BUT homeland, not using own data
			# --here the database selection is backformatted and a copy made for homeland function
			if ( (length(grep("1", optOUT)) == 0 & length(grep("4", optOUT)) > 0) & optLGS != "5" ) {
				backformat(database, "tmp_holman_in.txt")
			}
			# the choice was output for Holman programs AND NOT homeland, not using own data
			# --here the database selection is backformatted and a copy made for homeland function
			if ( (length(grep("1", optOUT)) > 0 & length(grep("4", optOUT)) == 0) & optLGS != "5" ) {
				backformat(database, outfile_holman)
			}
			# the choice was output for Holman programs, not using own data
			if ( length(grep("1", optOUT)) > 0 & optLGS != "5" ) {
				cat("An input file for Holman's software called", outfile_holman, "was produced.\n")
				if ( optREC=="1" ) {
					cat("An input file for Holman's software called", outfile_holman, "was produced\n", file=logfile, append=TRUE)
				}
			}
			# the choice was making a map
			if ( length(grep("2", optOUT)) > 0 ) {
				cat("\n\nYou now have one or more of the following options:\n\n")
				cat("    1. Produce a file with a topographical map\n")
				cat("       (this requires you to be online right now!)\n")
				cat("       --not a good option for a single language\n")
				cat("         or a map of several continents\n")
				cat("    2. Produce a simple pop-up map\n")
				cat("       (this can be saved by right-clicking and\n")
				cat("        should be clicked away after viewing/saving)\n")
				cat("\n\nType one or more numbers next to each other and press ENTER ")
				optMAPS <- readline()
				optMAPS <- insist(optMAPS, "12x"); regret(optMAPS)
				if ( length(grep("1", optMAPS)) > 0 ) {
					cat("\n\nYou can now choose a file name for the topographical map.\n")
					cat("This should carry the suffix .png.\n")
					cat("Or you can use choose the default name map.png by typing the letter D.\n")
					cat("If the default is chosen an existing file called map.png will be overwritten.\n")
					cat("\n\nNow type the file name or D and press ENTER ")
					optMAPFILE <- readline()
					if ( toupper(optMAPFILE)=="D" ) {
						optMAPFILE <- "map.png"
					} else {
						optMAPFILE <- insist_suffix(optMAPFILE, "png"); regret(optMAPFILE)
					}
				}
			}
			# the choices were to produce a map and not use own data
			if ( length(grep("2", optOUT)) > 0 & optLGS != "5" ) {
				m <- match(names, data_all$names)
				lats <- data_all[m,6]
				lons <- data_all[m,7]
			}
			# the choices were to produce a map and use own data
			if ( length(grep("2", optOUT)) > 0 & optLGS == "5" ) {
				cat("Type the name of your ASJP-style database file and press ENTER ")
				optMAPINPUTFILE <- readline()
				optMAPINPUTFILE <- insist_file(optMAPINPUTFILE); regret(optMAPINPUTFILE)
				parseASJP(optMAPINPUTFILE, "tmp_formatted.tab")
				tmp_data <- read.table(file="tmp_formatted.tab", header=TRUE, sep="\t", quote="", na.strings="", strip.white=TRUE, comment.char="", stringsAsFactors=FALSE, colClasses="character",numerals="no.loss")
				lats <- tmp_data[,6]
				lons <- tmp_data[,7]
				invisible(file.remove("tmp_formatted.tab"))
				rm(tmp_data)
			}
			# the choice was to produce a map
			if ( length(grep("2", optOUT)) > 0 ) {
				# the choice was to produce a topographical map
				if ( length(grep("1", optMAPS)) > 0 ) {
					topo_map(lats, lons, optMAPFILE)
				}
				# the choice was to produce a simple pop-up map
				if ( length(grep("2", optMAPS)) > 0 ) {
					simple_map(lats, lons)
				}
			}
			# choice was to compute LDN or LDND
			if ( length(grep("3", optOUT)) > 0 ) {
				cat("\n\nYou now have one or more of the following options:\n\n")
				cat("    0. Compute the standard ASJP distance (LDND),\n")
				cat("       outputting a list of pairwise doculects and distances.\n")
				cat("    1. Compute pairwise LDNs in MEGA format\n")
				cat("       --Recommended for a large set of doculects.\n")
				cat("       (See https://www.megasoftware.net/).\n")
				cat("    2. Compute pairwise LDNs in Splitstree format\n")
				cat("       --Recommended only for a set of up to some 100 doculects.\n")
				cat("       (See http://www.splitstree.org/).\n")
				cat("    3. Compute pairwise LDNs, displaying a phylogeny.\n")
				cat("       --Recommended for a set of up to some 30 doculects.\n")
				cat("    4. Compute pairwise LDNDs in MEGA format.\n")
				cat("       --Recommended for a large set of doculects.\n")
				cat("    5. Compute pairwise LDNDs in Splitstree format.\n")
				cat("       --Recommended only for a set of up to some 100 doculects.\n")
				cat("    6. Compute pairwise LDNDs, displaying a phylogeny.\n")
				cat("       --Recommended only for a set of up to some 30 doculects.\n")
				cat("    7. Compute pairwise LDNs in phylip format.\n")
				cat("       (See http://evolution.genetics.washington.edu/phylip.html).\n")
				cat("    8. Compute pairwise LDNDs in phylip format.\n\n")
				cat("    a. Compute pairwise LDNs in a standard matrix format.\n\n")
				cat("    b. Compute pairwise LDNDs in a standard matrix format.\n\n")
				cat("Type one or more numbers or letters next to each other and press ENTER ")
				optOUT_distances <- readline()
				optOUT_distances <- insist(optOUT_distances, "012345678abx"); regret(optOUT_distances)
			}
			if ( (length(grep("3", optOUT)) > 0 | length(grep("4", optOUT)) > 0) | length(grep("5", optOUT)) > 0 ) {
				# choice was to work with own transcriptions
				if ( optLGS=="5" ) {
					cat("Type the name of the file with your transcribed data and press ENTER ")
					optINPUTFILE <- readline()
					optINPUTFILE <- insist_file(optINPUTFILE); regret(optINPUTFILE)
					parseASJP(optINPUTFILE, "own_data_formatted.tab")
					own_data_prepared <- prepare_data("own_data_formatted.tab")
					data_words_own <- own_data_prepared[[1]]
					data_all_own <- own_data_prepared[[2]]
					if ( length(grep("4", optOUT)) > 0 ) {
						invisible(file.copy(optINPUTFILE, "tmp_holman_in.txt", overwrite=TRUE))
					}
				}
			}
			# choice was to compute LDN or LDND
			if ( length(grep("3", optOUT)) > 0 ) {
				# choice was to compute phylogenies; this requires phangorn
				if ( length(grep("3", optOUT_distances)) > 0 | length(grep("6", optOUT_distances)) > 0 ) {
					if ( ("phangorn" %in% installed.packages())==FALSE ) {
						install.packages("phangorn", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
					}
					if ( ("ape" %in% installed.packages())==FALSE ) {
						install.packages("ape", repos="http://cran.us.r-project.org", verbose=FALSE, quiet=TRUE)
					}
					suppressMessages(library(ape))
					suppressMessages(library(phangorn))
				}
				# choice was to compute LDNs in one format or the other
				matches <- match(c("1", "2", "3", "7", "a"), strsplit(optOUT_distances, "")[[1]])
				if ( length(which(is.na(matches))) < length(matches) ) {
					if ( optLGS!="5" ) {
						MEGA_format("LDN", "tmp_LDN_outfile.meg", names, data_words)
					}
					if ( optLGS=="5" ) {	
						MEGA_format("LDN", "tmp_LDN_outfile.meg", data_words_own[,1], data_words_own)
					}
				}
				# choice was to compute LDNs for MEGA
				if ( length(grep("1", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNs in MEGA format do you want to\n\n")
					cat("    1. use the default filename LDNs.meg, which will overwrite an existing file with that name\n")
					cat("    2. choose your own file name (where using the suffix .meg is required)\n\n")
					cat("Type a number and press ENTER ")
					optOUT_LDN_MEGA_filename <- readline()
					optOUT_LDN_MEGA_filename <- insist(optOUT_LDN_MEGA_filename, "12x"); regret(optOUT_LDN_MEGA_filename)
					if ( optOUT_LDN_MEGA_filename=="1" ) {
						LDN_mega_outfile <- "LDNs.meg"
					}
					if ( optOUT_LDN_MEGA_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDN_mega_outfile <- readline()
						LDN_mega_outfile <- insist_suffix(LDN_mega_outfile, "meg"); regret(LDN_mega_outfile)
					}
					invisible(file.copy("tmp_LDN_outfile.meg", LDN_mega_outfile, overwrite=TRUE))
					if ( optREC=="1" ) {
						cat("An input file with LDNs for MEGA called", LDN_mega_outfile, "was produced\n", file=logfile, append=TRUE)
					}
				}
				# choice was to compute LDNs for Splitstree
				if ( length(grep("2", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNs in Splitstree format do you want to\n\n")
					cat("    1. use the default filename LDNs.nex, which will overwrite an existing file with that name?\n")
					cat("    2. choose your own file name (where using the suffix .nex is required)?\n\n")
					cat("Type a number and press ENTER ")
					optOUT_LDN_splitstree_filename <- readline()
					optOUT_LDN_splitstree_filename <- insist(optOUT_LDN_splitstree_filename, "12x"); regret(optOUT_LDN_splitstree_filename)
					if ( optOUT_LDN_splitstree_filename=="1" ) {
						LDN_splitstree_outfile <- "LDNs.nex"
					}
					if ( optOUT_LDN_splitstree_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDN_splitstree_outfile <- readline()
						LDN_splitstree_outfile <- insist_suffix(LDN_splitstree_outfile, "nex"); regret(LDN_splitstree_outfile)
					}
					MEGA_to_splitstree("tmp_LDN_outfile.meg", LDN_splitstree_outfile)
				}
				# choice was to compute LDNs for phylip
				if ( length(grep("7", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNs in phylip format do you want to\n\n")
					cat("    1. use the default filename LDNs.phy, which will overwrite an existing file with that name?\n")
					cat("    2. choose your own file name?\n\n")			
					cat("Type a number and press ENTER ")
					optOUT_LDN_phylip_filename <- readline()
					optOUT_LDN_phylip_filename <- insist(optOUT_LDN_phylip_filename, "12x"); regret(optOUT_LDN_phylip_filename)
					if ( optOUT_LDN_phylip_filename=="1" ) {
						LDN_phylip_outfile <- "LDNs.phy"
					}
					if ( optOUT_LDN_phylip_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDN_phylip_outfile <- readline()
						LDN_phylip_outfile <- insist_overwrite_file(LDN_phylip_outfile); regret(LDN_phylip_outfile)
					}
					MEGA_to_phylip("tmp_LDN_outfile.meg", LDN_phylip_outfile)
				}
				# choice was to compute LDNs in a simple matrix format
				if ( length(grep("a", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNs in matrix format do you want to\n\n")
					cat("    1. use the default filename LDNs.mat, which will overwrite an existing file with that name?\n")
					cat("    2. choose your own file name?\n\n")			
					cat("Type a number and press ENTER ")
					optOUT_LDN_mat_filename <- readline()
					optOUT_LDN_mat_filename <- insist(optOUT_LDN_mat_filename, "12x"); regret(optOUT_LDN_mat_filename)
					if ( optOUT_LDN_mat_filename=="1" ) {
						LDN_mat_outfile <- "LDNs.mat"
					}
					if ( optOUT_LDN_mat_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDN_mat_outfile <- readline()
						LDN_mat_outfile <- insist_overwrite_file(LDN_mat_outfile); regret(LDN_mat_outfile)
					}
					MEGA_to_matrix("tmp_LDN_outfile.meg", LDN_mat_outfile)
				}
				# choice was to produce a phylogeny from LDNs
				if ( length(grep("3", optOUT_distances)) > 0 ) {
					cat("\n\nFor the pairwise LDNs do you want to\n\n")
					cat("    1. see a Neighbor-Joining tree?\n")
#					cat("    2. see a NeighborNet?\n\n")	
					cat("Type a number and press ENTER ")
					optPHYLOGENY <- readline()
					optPHYLOGENY <- insist(optPHYLOGENY, "1x"); regret(optPHYLOGENY)
					# choice was a Neighbor-Joining tree
					if ( optPHYLOGENY=="1" | optPHYLOGENY=="2" ) {
						MEGA_to_phylip("tmp_LDN_outfile.meg")
						read_phylip <- scan("phylip_in.txt", what="character", quiet=TRUE)
						no_taxa <- as.numeric(read_phylip[1])
						if ( no_taxa == 2 ) {
							cat("I cannot make a graph of just two taxa\n")
						} else if ( no_taxa == 1 ) {
							cat("I cannot make a graph of just one taxon\n")
						} else {
							# choice was Neighbor-Joining
							if ( optPHYLOGENY=="1" ) {
								plot(NJ(readDist("phylip_in.txt")))
							}
#							# choice was a NeighborNet
#							if ( optPHYLOGENY=="2" ) {
#								plot(neighborNet(readDist("phylip_in.txt")))
#							}
							cat("The plot should be clicked away after viewing, but\n")
							cat("the image can be saved first through right-clicking on it.\n")
						}
					}
					if ( "phylip_in.txt" %in% dir() ) {
						invisible(file.remove("phylip_in.txt"))
					}
				}
				if ( "tmp_LDN_outfile.meg" %in% dir() ) {
					invisible(file.remove("tmp_LDN_outfile.meg"))
				}
			}
			# choice was to compute LDNDs in one format or the other
			if ( length(grep("3", optOUT)) > 0 ) {
				# choice was to compute LDNDs in one of six different formats
				matches <- match(c("0", "4", "5", "6", "8", "b"), strsplit(optOUT_distances, "")[[1]])
				if ( length(which(is.na(matches))) < length(matches) ) {
					if ( optLGS!="5" ) {
						MEGA_format("LDND", "tmp_LDND_outfile.meg", names, data_words)
					}
					if ( optLGS=="5" ) {
						MEGA_format("LDND", "tmp_LDND_outfile.meg", data_words_own[,1], data_words_own)
					}
				cat("", file="LDNDs_computed")
				}
			}
			# choice was to compute a homeland or do dates
			if ( length(grep("4", optOUT)) > 0 | length(grep("5", optOUT)) > 0 ) {
				if ( length(which(dir()=="LDNDs_computed"))==0 ) {
					if ( optLGS!="5" ) {
						MEGA_format("LDND", "tmp_LDND_outfile.meg", names, data_words)
					}
					if ( optLGS=="5" ) {
						MEGA_format("LDND", "tmp_LDND_outfile.meg", data_words_own[,1], data_words_own)
					}
				}
			}
			if ( length(which(dir()=="LDNDs_computed")) > 0 ) {
				invisible(file.remove("LDNDs_computed"))
			}
			if ( length(grep("3", optOUT)) > 0 ) {
				# choice was to compute LDNDs outputting a list of distances
				if ( length(grep("0", optOUT_distances)) > 0 ) {
					cat("\n\nYou should now choose an file name for your output.\n")
					cat("Please check first that it is not already in use.\n")
					cat("\nType the file name and press ENTER ")
					LDND_simple_outfile <- readline()
					LDND_simple_outfile <- insist_overwrite_file(LDND_simple_outfile); regret(LDND_simple_outfile)
					MEGA_to_simple("tmp_LDND_outfile.meg", LDND_simple_outfile)
				}
				# choice was to compute LDNDs for MEGA
				if ( length(grep("4", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNDs in MEGA format, do you want to\n\n")
					cat("    1. use the default filename LDNDs.meg, which will overwrite an existing file with that name\n")
					cat("    2. choose your own file name (where using the suffix .meg is required)\n\n")
					cat("Type a number and press ENTER ")
					optOUT_LDND_MEGA_filename <- readline()
					optOUT_LDND_MEGA_filename <- insist(optOUT_LDND_MEGA_filename, "12x"); regret(optOUT_LDND_MEGA_filename)
					if ( optOUT_LDND_MEGA_filename=="1" ) {
						LDND_mega_outfile <- "LDNDs.meg"
					}
					if ( optOUT_LDND_MEGA_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDND_mega_outfile <- readline()
						LDND_mega_outfile <- insist_suffix(LDND_mega_outfile, "meg"); regret(optOUT_LDND_MEGA_filename)
					}
					invisible(file.copy("tmp_LDND_outfile.meg", LDND_mega_outfile, overwrite=TRUE))
					if ( optREC=="1" ) {
						cat("An input file with LDNDs for MEGA called", LDND_mega_outfile, "was produced\n", file=logfile, append=TRUE)
					}
				}
				# choice was to compute LDNDs for Splitstree
				if ( length(grep("5", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNDs in Splitstree format do you want to\n\n")
					cat("    1. use the default filename LDNDs.nex, which will overwrite an existing file with that name\n")
					cat("    2. choose your own file name (where using the suffix .nex is required)\n\n")			
					cat("Type a number and press ENTER ")
					optOUT_LDND_splitstree_filename <- readline()
					optOUT_LDND_splitstree_filename <- insist(optOUT_LDND_splitstree_filename, "12x"); regret(optOUT_LDND_splitstree_filename)
					if ( optOUT_LDND_splitstree_filename=="1" ) {
						LDND_splitstree_outfile <- "LDNDs.nex"
					}
					if ( optOUT_LDND_splitstree_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDND_splitstree_outfile <- readline()
						LDND_splitstree_outfile <- insist_suffix(LDND_splitstree_outfile, "nex"); regret(LDND_splitstree_outfile)
					}
					MEGA_to_splitstree("tmp_LDND_outfile.meg", LDND_splitstree_outfile)
				}
				# choice was to compute LDNDs for phylip
				if ( length(grep("8", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNDs in phylip format do you want to\n\n")
					cat("    1. use the default filename LDNDs.phy, which will overwrite an existing file with that name?\n")
					cat("    2. choose your own file name?\n\n")
					cat("Type a number and press ENTER ")
					optOUT_LDND_phylip_filename <- readline()
					optOUT_LDND_phylip_filename <- insist(optOUT_LDND_phylip_filename, "12x"); regret(optOUT_LDND_phylip_filename)
					if ( optOUT_LDND_phylip_filename=="1" ) {
						LDND_phylip_outfile <- "LDNDs.phy"
					}
					if ( optOUT_LDND_phylip_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDND_phylip_outfile <- readline()
						LDND_phylip_outfile <- insist_overwrite_file(LDND_phylip_outfile); regret(LDND_phylip_outfile)
					}
					MEGA_to_phylip("tmp_LDND_outfile.meg", LDND_phylip_outfile)
				}
				# choice was to compute LDNDs in a matrix format
				if ( length(grep("b", optOUT_distances)) > 0 ) {
					cat("\n\nFor the LDNDs in matrix format do you want to\n\n")
					cat("    1. use the default filename LDNDs.mat, which will overwrite an existing file with that name?\n")
					cat("    2. choose your own file name?\n\n")
					cat("Type a number and press ENTER ")
					optOUT_LDND_mat_filename <- readline(); regret(optOUT_LDND_mat_filename)
					if ( optOUT_LDND_mat_filename=="1" ) {
						LDND_mat_outfile <- "LDNDs.mat"
					}
					if ( optOUT_LDND_mat_filename=="2" ) {
						cat("Type the file name and press ENTER ")
						LDND_mat_outfile <- readline()
						LDND_mat_outfile <- insist_overwrite_file(LDND_mat_outfile); regret(LDND_mat_outfile)
					}
					MEGA_to_matrix("tmp_LDND_outfile.meg", LDND_mat_outfile)
				}
				# choice was to produce a phylogeny from LDNDs
				if ( length(grep("6", optOUT_distances)) > 0 ) {
					cat("\n\nFor the pairwise LDNDs do you want to\n\n")
					cat("    1. see a Neighbor-Joining tree?\n")
#					cat("    2. see a NeighborNet?\n\n")
					cat("Type a number and press ENTER ")
					optPHYLOGENY <- readline()
					optPHYLOGENY <- insist(optPHYLOGENY, "1x"); regret(optPHYLOGENY)
					# choice was a Neighbor-Joining tree
					if ( optPHYLOGENY=="1" | optPHYLOGENY=="2") {
						MEGA_to_phylip("tmp_LDND_outfile.meg")
						read_phylip <- scan("phylip_in.txt", what="character", quiet=TRUE)
						no_taxa <- as.numeric(read_phylip[1])
						if ( no_taxa == 2 ) {
							cat("I cannot make a graph of just two taxa\n")
						} else if ( no_taxa == 1 ) {
							cat("I cannot make a graph of just one taxon\n")
						} else {
							# choice was Neighbor-Joining
							if ( optPHYLOGENY=="1" ) {
								plot(NJ(readDist("phylip_in.txt")))
							}
#							# choice was a NeighborNet
#							if ( optPHYLOGENY=="2" ) {
#								plot(neighborNet(readDist("phylip_in.txt")))
#							}
							cat("The plot should be clicked away after viewing, but\n")
							cat("the image can be saved first through right-clicking on it.\n")
						}
					}
#					if ( optPHYLOGENY=="2" ) {
#						MEGA_to_phylip("tmp_LDND_outfile.meg")
#						plot(neighborNet(readDist("phylip_in.txt")))
#					}
					if ( "tmp_LDN_outfile.meg" %in% dir() ) {
						invisible(file.remove("phylip_in.txt"))
					}
				}
			}
			# the choice was to infer a homeland
			if ( length(grep("4", optOUT)) > 0 ) {
				cat("\n\nYou can now choose a name for the file with coordinates\n")
				cat("or choose the default name homeland.txt by typing the letter D.\n")
				# cat("\n\nYou can now choose file names for the map(s) of homeland(s)\n")
				# cat("and for the output of coordinates. Only give the main name, not the suffix.\n")
				# cat("E.g.: myfile. Two files will be produced, e.g., myfile.pdf and myfile.txt.\n")
				# cat("Or choose the default names homeland.pdf and homeland.txt by typing the letter D.\n")
				# cat("If the default is chosen existing files with the same names will be overwritten.\n")
				cat("\n\nNow type the file name or D and press ENTER ")
				optHOMELANDFILE <- readline()
				optHOMELANDFILE <- insist_overwrite_file(optHOMELANDFILE); regret(optHOMELANDFILE)
				if ( toupper(optHOMELANDFILE)=="D" ) {
					# homelandPDFFile <- "homeland.pdf"
					homelandTXTFile <- "homeland.txt"
				} else {
					# homelandPDFFile <- paste(optHOMELANDFILE, ".pdf", sep="")
					homelandTXTFile <- optHOMELANDFILE
				}
				# homeland(homelandTXTFile, homelandPDFFile, optCLASS)
				homeland(homelandTXTFile, optCLASS)
			}
			# the choice was to infer a date
			if ( length(grep("5", optOUT)) > 0 ) {
				MEGA_to_matrix("tmp_LDND_outfile.meg", "tmp_mat.txt")
				m <- read.table(file="tmp_mat.txt", header=TRUE, row.names=1)
				invisible(file.remove("tmp_mat.txt"))
				if ( optLGS!="5" ) {
					w_names <- match(names, data_all$names)
					if ( optCLASS=="1" ) {
						cls <- paste(data_all$hh[w_names], data_all$iso[w_names], sep=",")
					}
					if ( optCLASS=="2" ) {
						cls <- paste(data_all$wls_fam[w_names], data_all$wls_gen[w_names], data_all$iso[w_names], sep=",")
					}
					if ( optCLASS=="3" ) {
						cls <- paste(data_all$e[w_names], data_all$iso[w_names], sep=",")
					}
				}
				if ( optLGS=="5" ) {
					names <- data_all_own[,1]
					w_names <- match(names, data_all_own$names)
					if ( optCLASS=="1" ) {
						cls <- paste(data_all_own$hh[w_names], data_all_own$iso[w_names], sep=",")
					}
					if ( optCLASS=="2" ) {
						cls <- paste(data_all_own$wls_fam[w_names], data_all_own$wls_gen[w_names], data_all_own$iso[w_names], sep=",")
					}
					if ( optCLASS=="3" ) {
						cls <- paste(data_all_own$e[w_names], data_all_own$iso[w_names], sep=",")
					}
				}
				# the choice was families
				if ( optLGS=="2" ) {  # family
					lev1 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][1])))
					lev2 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][2])))
				}
				# the choice was major subgroups
				if ( optLGS=="3" ) {  # major subgroups
					lev1 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][2])))
					lev2 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][3])))
				}
				# the choice was a 1. list, 4. a subset of the entire database, 5. own database
				if ( ((optLGS=="1" | optLGS=="4") | optLGS=="5") ) { 
					ID_lev <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][1])))
					# if the first level entities are different families they are each dated 
					# by the next subgroups down, or ISO-codes, if that's the next level
					if ( length(unique(ID_lev)) > 1 ) {
						lev1 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][1])))
						lev2 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][2])))
					}
					# If all the ISO-codes are the same level 2 will be that of the doculects
					# and level 1 the preceding one
					if ( length(unique(data_all$iso[w_names]))==1 ) {
						lev1 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][length(strsplit(cls[1], ",")[[1]])-1])))
						lev2 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][length(strsplit(cls[1], ",")[[1]])])))

					}
					# If there are doculects belonging to different ISO-codes but the top-level family
					# is shared, one would need to find identify level 2 with the first level
					# where different groups appear, and level 1 with the preceding level
					if ( length(unique(ID_lev)) == 1 & length(unique(data_all$iso[w_names])) > 1 ) {
						same <- TRUE
						L <- 2
						while ( same==TRUE ) {
							lev1 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][L-1])))
							lev2 <- as.vector(unlist(lapply(cls, function(x) strsplit(x, ",")[[1]][L])))
							if ( length(unique(lev2))==1 ) {
								same <- TRUE
								L <- L + 1
							} else {
								same <- FALSE
							}
						}
					}
				}

				d <- data.frame(names, lev1, lev2)
				for (i in 1:length(levels(as.factor(d$lev1)))) {
					current <- d[which(d$lev1==levels(as.factor(d$lev1))[i]),]
					tab <- cbind(expand.grid(current$names, current$names), expand.grid(current$lev2, current$lev2))
					dists <- c()
					count <- 0
					if ( identical(tab[,3], tab[,4])==FALSE ) {
						for (j in 1:length(tab[,1])) {
							if ( tab[j,3] != tab[j,4] ) {
								count <- count + 1
								dists[count] <- m[tab[j,1], tab[j,2]]
							}
						}
					}
					# this is the case where the set only include doculects
					# belonging to one and the same ISO-code
					if ( identical(tab[,3], tab[4])==TRUE ) {
						for (j in 1:length(tab[,1])) {
							count <- count + 1
							dists[count] <- m[tab[j,1], tab[j,2]]
						}
					}
					pairs <- count/2
					family <- levels(as.factor(d$lev1))[i]
					mean_dist <- mean(dists)
					mean_sim <- round(100*(1 - mean_dist), 2)
					age <- round(1000*((log10(((100-mean_dist*100)/100))-log10(0.92))/(2*log10(0.72))),0)
					cat("\nfamily/group:", family, "\n")
					cat("no. pairs:", pairs, "\n")
					cat("similarity:", mean_sim, "\n")
					cat("age (before present):", age, "\n")
				}
			}
		}
		# choice was to transcribe own data
		if ( optDATA=="2" ) {
			cat("\n\nYou should be working with 100-item word lists in Excel.\n")
			cat("   even you only enter words for the 40-item subset\n\n")
			cat("The default encoding is unicode IPA.\n\n")
			cat("See the file example_own_data.xlsx for how to format the file.\n\n")
			cat("All fields except the language name are optional,\n")
			cat("   but supplying an ISO-code is strongly recommended.\n\n")
			# input file name prompt
			cat("Please type the name of your input file and press ENTER ")
			optINFILE <- readline()
			optINFILE <- insist_file(optINFILE); regret(optINFILE)
			cat("Type the number of word lists your file contains and press ENTER ")
			optNOLISTS <- readline()
			optNOLISTS <- insist_wordlists(optNOLISTS); regret(optNOLISTS)
			trASJP(optINFILE, optNOLISTS)  # function in ASJP_transcriber.R
		}
	}
	# choice was to enter the editor's corner
	if ( optAB=="3" ) {
		cat("\n\nDo you want to\n\n")
		cat("    1. Check an ASJP-style database or wordlist for issues?\n")
		cat("    2. Get statistics on an ASJP-style database?\n")
		cat("    3. Produce a tab-delimited file from an ASJP-style database?\n")
		cat("    4. Update the Glottolog metadata accessible to this program?\n")
		cat("    5. Generate a file with metadata for missing languages?\n")
		cat("\n\nType one or more numbers next to each other and press ENTER ")
		optEDITOR <- readline()
		optEDITOR <- insist(optEDITOR, "12345x"); regret(optEDITOR)
		# choice was to work with an ASJP-style database
		if ( (length(grep("1", optEDITOR))==1 | length(grep("2", optEDITOR))==1) | length(grep("3", optEDITOR))==1 ) {
			cat("\nPlease type the name of your file and press ENTER ")
			optFILEEDIT <- readline()
			optFILEEDIT <- insist_file(optFILEEDIT); regret(optFILEEDIT)
		}
		# choice was to check a file for issues or get statistics
		if ( length(grep("1", optEDITOR))==1 | length(grep("2", optEDITOR))==1 ) {
			cdata <- suppressWarnings(readLines(optFILEEDIT))
		}
		# choice was to check a database or word list for issues
		if ( length(grep("1", optEDITOR))==1 ) {
			cat("Issues will be recorded in a file called", paste("issues_", optFILEEDIT, sep=""),".\n")
			issues_file <- paste("issues_", optFILEEDIT, sep="")
			cat("", file=issues_file)
		}
		# choice was to check a file for issues or get statistics
		if ( length(grep("1", optEDITOR))==1 | length(grep("2", optEDITOR))==1 ) {
			cat("\n\nIs the file you want to check\n\n")
			cat("    1. A set of one or more word lists?\n")
			cat("    2. An ASJP-style database with preamble?\n\n")
			cat("Type a number and press ENTER ")
			optDATATYPE <- readline()
			optDATATYPE <- insist(optDATATYPE, "12x"); regret(optDATATYPE)
			preamble <- readLines("preamble.txt")
			# choice was to check a file for issues and the type is an ASJP-style database with preamble
			if ( length(grep("1", optEDITOR))==1 & length(grep("2", optDATATYPE))==1 ) {
				cat("\nChecking the preamble...\n")
			}
			# check if it there
			cp <- 0  # stands for confidence in presence of preamble
			if ( length(grep("     2    28  1700     1    92    72                       ", cdata[1])) > 0 ) {
				cp <- cp + 1
			}
			if ( length(grep("(I4,20X,10A1)", cdata[2])) > 0 ) {
				cp <- cp + 1
			}
			if ( length(grep("   1                    I" , cdata[3])) > 0 ) {
				cp <- cp + 1
			}
			# choice was to check a file for issues
			if ( length(grep("1", optEDITOR))==1 & length(grep("2", optDATATYPE))==1) {
				if ( cp > 0 & identical(cdata[1:86],preamble) ) {
					cat("  The preamble is ok.\n")
					cat("\nThe preamble is ok.\n", file=issues_file, append=TRUE)
				} else if ( cp == 0 ) {
					cat("  The preamble is missing.\n")
					cat("\nThe preamble is missing.\n", file=issues_file, append=TRUE)
				} else if ( cp > 0 & !identical(cdata[1:86],preamble) ) {
					cat("  The preamble is not exactly as the standard template.\n")
					cat("\nThe preamble is not exactly as the standard template.\n", file=issues_file, append=TRUE)
					for (i in 1:86) {
						if ( !identical(cdata[i], preamble[i]) ) {
							cat("    - line",i,"differs from the standard template.\n")
							cat("   - line",i,"differs from the standard template.\n", file=issues_file, append=TRUE)
						}
					}
				}
			}
			# get rid of the preamble if it is there
			if ( cp > 0 ) {
				end_preamble <- 86
				cdata <- cdata[-c(1:end_preamble)]
			}
			last_data_line <- max(grep("^[[:digit:]]", cdata))
			firstlines <- cdata[grep("^ [[:digit:]]", cdata) - 1]
			secondlines <- cdata[grep("^ [[:digit:]]", cdata)]
			w_first <- grep("^ [[:digit:]]", cdata) - 1
			w_second <- grep("^ [[:digit:]]", cdata)
			first_data_line <- w_second[1] + 1
			datalines <- setdiff(c(first_data_line:last_data_line),c(w_first,w_second))
			wls_fam_all <- as.vector(unlist(lapply(firstlines, get_wls_fam)))
			wls_fam_all_unique <- unique(wls_fam_all)
			wls_gen_all <- as.vector(unlist(lapply(firstlines, get_wls_cls)))
			wls_gen_all_unique <- unique(wls_gen_all)
			ecs <- as.vector(unlist(lapply(firstlines, get_e_cls)))
			iss <- as.vector(unlist(lapply(secondlines, get_iso_code)))
			pos <- as.vector(unlist(lapply(secondlines, get_population)))
			nas <- as.vector(unlist(lapply(firstlines, get_name)))
			gcs <- as.vector(unlist(lapply(firstlines, get_g_cls)))
			was <- as.vector(unlist(lapply(secondlines, get_wls_code)))
			if ( !exists("ethn") ) {
				ethn <<- read.table("e24.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
			}
		}
		# choice was to check a file for issues
		if ( length(grep("1", optEDITOR))==1 ) {
			cat("\nDo you want me to include a check of the data lines?\n")
			cat("\n   1. Yes.\n   2. No.\n\nType the number and press ENTER ")
			optCHECKDATA <- readline()
			optCHECKDATA <- fix_yn(optCHECKDATA); optCHECKDATA <- insist(optCHECKDATA, "12x"); regret(optCHECKDATA)
			if ( optDATATYPE=="2" ) {
				cat("\nChecking the end of the file...\n")
				if ( length(cdata)==last_data_line ) {
					cat("  You need to add a line with five empty spaces\n")
					cat("   plus newline to the end\n")
					cat("\nIf this is a databse you need to add a line with five empty space plus newline to the end.\n", file=issues_file, append=TRUE)
				} else if ( length(grep("     ", cdata[last_data_line + 1]))==0 ) {
					cat("  You need to add a line\n")
					cat("    with five empty spaces plus newline to the end\n")
					cat("\nYou need to add a line with five empty space plus newline to the end.\n", file=issues_file, append=TRUE)
				} else {
					cat("  The end of the file is ok.\n")
					cat("\nThe end of the file is ok.\n", file=issues_file, append=TRUE)
				}
			}
			cat("\nChecking for hyphens in doculect names...\n")
			badnames <- nas[grep("-", nas)]
			if ( length(badnames) > 0 ) {
				for (i in 1:length(badnames) ) {
					if ( i == 1 ) {
						cat("\n", file=issues_file, append=TRUE)
					}
					cat("  The doculect name", badnames[i], "has a hyphen in it.\n")
					cat("The doculect name", badnames[i], "has a hyphen in it.\n", file=issues_file, append=TRUE)
				}
			} else {
				cat("  No hyphens in doculect names found.\n")
				cat("\nNo hyphens in doculect names found.\n", file=issues_file, append=TRUE)
			}
			cat("\nChecking for spaces in the first line of metadata...\n")
			badlines <- firstlines[grep(" ", firstlines)]
			if ( length(badlines) > 0 ) {
				for (i in 1:length(badlines) ) {
					if ( i == 1 ) {
						cat("\n", file=issues_file, append=TRUE)
					}
					cat("  The first line for", strsplit(badlines[i], "\\{")[[1]][1], "has one or more spaces.\n")
					cat("The first line for", strsplit(badlines[i], "\\{")[[1]][1], "has one or more spaces.\n", file=issues_file, append=TRUE)
				}
			} else {
				cat("  No spaces in the first line of metadata found.\n")
				cat("\nNo spaces in the first line of metadata found.\n", file=issues_file, append=TRUE)
			}
			cat("\nChecking the structure of the first line of metadata...\n")
			bados <- 0
			if ( length(grep("|", firstlines)) < length(firstlines) ) {
				bados <- bados + 1
				count <- 0
				for (i in 1:length(firstlines)) {
					if ( length(grep("|", firstlines[i]))==0 ) {
						count <- count + 1
						cat("  The line for", strsplit(firstlines[i], "\\{")[[1]][1], "is missing an |.\n")
						if ( count == 1 ) {
							cat("\n", file=issues_file, append=TRUE)
						}
						cat("The line for", strsplit(firstlines[i], "\\{")[[1]][1], "is missing an |.\n", file=issues_file, append=TRUE)
					}
				}
			}			
			if ( length(grep("@", firstlines)) == 0 ) {
				cat("  All first lines are missing an @.\n")
				cat("  All first lines are missing an @.\n", file=issues_file, append=TRUE)
				bados <- bados + 1
			} else {
				if ( length(grep("@", firstlines)) < length(firstlines) ) {
					bados <- bados + 1
					count <- 0
					for (i in 1:length(firstlines)) {
						if ( length(grep("@", firstlines[i]))==0 ) {
							count <- count + 1
							if ( count == 1 ) {
								cat("\n", file=issues_file, append=TRUE)
							}
							cat("  The line for", strsplit(firstlines[i], "\\{")[[1]][1], "is missing an @.\n")
							cat("The line for", strsplit(firstlines[i], "\\{")[[1]][1], "is missing an @.\n", file=issues_file, append=TRUE)
						}
					}
				}			
			}
			if ( bados==0 ) {
				cat("  The structure of the first line of metadata is ok.\n")
				cat("\nThe structure of the first line of metadata is ok.\n", file=issues_file, append=TRUE)
			}
			w_start_fam <- match(wls_fam_all_unique, wls_fam_all)
			w_start_gen <- match(wls_gen_all_unique, wls_gen_all)
			if ( optDATATYPE=="2" ) {
				cat("\nNow on to alphabetical orders of genera and doculects\n")
				cat("\nChecking the alphabetical order of genera...\n")
				count <- 0
				bados <- 0
				for (i in 1:length(w_start_fam)) {
					if ( i < length(w_start_fam) ) {
						genera <- as.vector(unlist(lapply(firstlines[w_start_fam[i]:(w_start_fam[i+1] - 1)], get_wls_gen)))
						doculects <- as.vector(unlist(lapply(firstlines[w_start_fam[i]:(w_start_fam[i+1] - 1)], get_doculect)))
					} else {
						genera <- as.vector(unlist(lapply(firstlines[w_start_fam[i]:length(firstlines)], get_wls_gen)))
						doculects <- as.vector(unlist(lapply(firstlines[w_start_fam[i]:length(firstlines)], get_doculect)))
					}
					out_of_order <- alphabetical_order_genera(genera, doculects)
					if ( length(out_of_order[[1]]) > 0 & length(out_of_order[[2]]) > 0 ) {
						for (j in 1:length(out_of_order[[1]])) {
							count <- count + 1
							bados <- bados + 1
							if ( count == 1 ) {
								cat("  Cases where the genera are not in alhabetical order:\n")
								cat("\nCases where the genera are not in alhabetical order:\n", file=issues_file, append=TRUE)
							}
							cat(out_of_order[[1]][j], "\t", out_of_order[[2]][j], "\n")
							cat(out_of_order[[1]][j], "\t", out_of_order[[2]][j], "\n", file=issues_file, append=TRUE)
						}
					cat("\n")
					cat("\n", file=issues_file, append=TRUE)
					}
				}
				if ( bados == 0 ) {
					cat("  The alphabetical order of genera is ok.\n")
					cat("\nThe alphabetical order of genera is ok.\n", file=issues_file, append=TRUE)
				}
				cat("\nChecking the alphabetical order of languages...\n")
				count <- 0
				bados <- 0
				for (i in 1:length(w_start_gen)) {
					if ( i < length(w_start_gen) ) {
						doculects <- as.vector(unlist(lapply(firstlines[w_start_gen[i]:(w_start_gen[i+1] - 1)], get_doculect)))
					} else {
						doculects <- as.vector(unlist(lapply(firstlines[w_start_gen[i]:length(firstlines)], get_doculect)))
					}
					out_of_order <- alphabetical_order_doculects(doculects)
					if ( length(out_of_order) > 0 ) {
						count <- count + 1
						bados <- bados + 1
						if ( count == 1 ) {
							cat("  Cases where the doculects are not in alhabetical order:\n")
							cat("\nCases where the doculects are not in alhabetical order:\n", file=issues_file, append=TRUE)
						}
						for (j in 1:length(out_of_order)) {
							cat(out_of_order[j], "\n")
							cat(out_of_order[j], "\n", file=issues_file, append=TRUE)
						}
					cat("\n")
					cat("\n", file=issues_file, append=TRUE)
					}
				}
				if ( bados == 0 ) {
					cat("  The alphabetical order of doculects is ok.\n")
					cat("\nThe alphabetical order of doculects is ok.\n", file=issues_file, append=TRUE)
				}
			}
			cat("\nChecking for non-ascii characters in first line of metadata...\n")
			count <- 1
			bados <- 0
			for (i in 1:length(firstlines)) {
				if ( length(grepNonASCII(firstlines[i])) > 0 ) {
					bados <- bados + 1
					count <- count + 1
					if ( count == 1 ) {
						cat("\n", file=issues_file, append=TRUE)
					}
					cat("  The first line for", strsplit(firstlines[i], "\\{")[[1]][1], "has a non-ascii character.\n")
					cat("The first line for", strsplit(firstlines[i], "\\{")[[1]][1], "has a non-ascii character.\n", file=issues_file, append=TRUE)
				}
			}
			if ( bados==0 ) {
				cat("  No non-ascii characters were found in the first line of metadata.\n")
				cat("\nNo non-ascii characters were found in the first line of metadata.\n", file=issues_file, append=TRUE)
			}
			if ( optDATATYPE=="2" ) {
				cat("\nChecking that the file FamilyAbbreviations.txt is up-to-date\n")
				bados <- 0
				count <- count + 1
				ufa <- unique(fam_abbr[,2])
				if ( length(fam_abbr[,2]) > length(ufa) ) {
					bados <- bados + 1
					for (i in 1:length(ufa)) {
						m <- length(which(fam_abbr[,2]==ufa[i]))
						if ( m > 1 ) {
							count <- count + 1
							cat("The abbreviation", ufa[i], "is used", m, "times\n")
							if ( count == 1 ) {
								cat("\n", file=issues_file, append=TRUE)
							}
							cat("  The abbreviation", ufa[i], "is used", m, "times\n", file=issues_file, append=TRUE)
						}
					}
				}
				if ( bados == 0 ) {
					cat("  There are no multiple uses of an abbreviation in FamilyAbbreviations.txt\n")
					cat("\nThere are no multiple uses of an abbreviation in FamilyAbbreviations.txt\n", file=issues_file, append=TRUE)
				}
				bados <- 0
				db_not_abbr <- setdiff(wls_fam_all_unique, ufa)
				abbr_not_db  <- setdiff(ufa, wls_fam_all_unique)
				if ( length(db_not_abbr) > 0 ) {
					bados <- bados + 1
					cat("  The following family abbreviations are in the database but not in FamilyAbbreviations.txt:\n")
					print(db_not_abbr)
					cat("\n")
					cat("\nThe following family abbreviations are in the database but not in FamilyAbbreviations.txt:\n", file=issues_file, append=TRUE)
					cat(db_not_abbr, sep=",", file=issues_file, append=TRUE)
					cat("\n", file=issues_file, append=TRUE)
				}
				if ( bados == 0 ) {
					cat("  The family abbreviations in the database are all in FamilyAbbreviations.txt\n")
					cat("\nThe family abbreviations in the database are all in FamilyAbbreviations.txt\n", file=issues_file, append=TRUE)
				}
				bados2 <- 0
				if ( length(abbr_not_db) > 0 ) {
					bados2 <- bados2 + 1
					cat("The following abbreviations are in FamilyAbbreviations.txt but not the database:\n")
					cat("  (but they might be relevant for another version of the database)\n")
					print(abbr_not_db)
					cat("\n")
					cat("\nThe following abbreviations are in FamilyAbbreviations.txt but not the database:\n", file=issues_file, append=TRUE)
					cat("(but they might be relevant for another version of the database)\n", file=issues_file, append=TRUE)
					cat(abbr_not_db, file=issues_file, append=TRUE)
					cat("\n", file=issues_file, append=TRUE)
				}
				if ( bados==0 & bados2==0) {
					cat("  FamilyAbbreviations.txt is completely up-to-date\n\n")
					cat("\nFamilyAbbreviations.txt is completely up-to-date\n\n", file=issues_file, append=TRUE)
				}
				if ( bados==0 & bados2==1) {
					cat("  FamilyAbbreviations.txt is sufficiently up-to-date\n\n")
					cat("\nFamilyAbbreviations.txt is sufficiently up-to-date\n\n", file=issues_file, append=TRUE)
				}
			}
			cat("\nNow checking the WALS classification...\n")
			cat("  I will go to the online WALS so you should be online.\n")
			cat("  This may take 15 minutes for a full database.\n")
			cat("  Do you want to\n")
			cat("    1. Go through with this step?\n")
			cat("    2. Skip it?\n")
			cat("Please type a number and press ENTER ")
			optWALS <- readline()
			optWALS <- fix_yn(optWALS); optWALS <- insist(optWALS, "12x"); regret(optWALS)
			# choice was to go through with checking the WALS classification
			if ( optWALS=="1" ) {
				bados <- 0
				# the following gives a list of lists of
				# family in ASJP, family in WALS
				# genus in ASJP, genus in WALS, all as is
				fs <- cbind(firstlines, secondlines)
				wci <- apply(fs, 1, class_WALS)  # stands for WALS classification info
				# going through families
				count <- 0
				for (i in 1:length(wci)) {
					if ( length(wci[[i]]) > 1 ) {
						if ( length(wci[[i]][1][[1]])==0 ) {
							cat("  Please update FamilyAbbreviations for", wci[[i]][[2]][1], "\n\n")
							cat("Please update FamilyAbbreviations for", wci[[i]][[2]][1], "\n", file=issues_file, append=TRUE)
							bados <- bados + 1
						}
						if ( length(wci[[i]][1][[1]])!=0 ) {
							if ( tolower(wci[[i]][1][[1]]) != tolower(wci[[i]][2][[1]]) ) {
								count <- count + 1
								if ( count == 1 ) {
									cat("  WALS families:\n\n")
									cat("\nWALS families:\n\n", file=issues_file, append=TRUE)
								}
								cat("  There is a discrepancy for families for", get_doculect(firstlines[i]), "\n")
								cat("  ASJP has", wci[[i]][1][[1]], "WALS has", wci[[i]][2][[1]], "\n\n")
								cat("There is a discrepancy for families for", get_doculect(firstlines[i]), file=issues_file, append=TRUE)
								cat("  ASJP has", wci[[i]][1][[1]], "WALS has", wci[[i]][2][[1]], "\n", file=issues_file, append=TRUE)
								bados <- bados + 1
							}
						}
					}
				}
				# going through genera
				count <- 0
				for (i in 1:length(wci)) {
					if ( length(wci[[i]]) > 1 ) {
						if ( tolower(wci[[i]][3][[1]]) != gsub(" ", "_", simplify(wci[[i]][4][[1]])) ) {
							count <- count + 1
							if ( count == 1 ) {
								cat("  WALS genera:\n\n")
								cat("\nWALS genera:\n\n", file=issues_file, append=TRUE)
							}
							cat("  There is a discrepancy for genera for", get_doculect(firstlines[i]), "\n")
							cat("  ASJP has", toupper(wci[[i]][3][[1]]), "WALS has", toupper(wci[[i]][4][[1]]), "\n\n")
							cat("There is a discrepancy for genera for", get_doculect(firstlines[i]), file=issues_file, append=TRUE)
							cat("  ASJP has", toupper(wci[[i]][3][[1]]), "WALS has", toupper(wci[[i]][4][[1]]), "\n", file=issues_file, append=TRUE)
							bados <- bados + 1
						}
					}
				}
				if ( bados==0 ) {
					cat("  No issues were found with the WALS classification.\n")
					cat("\nNo issues were found with the WALS classification.\n", file=issues_file, append=TRUE)
				}
			}
			cat("\nNow checking the Ethnologue classification...\n")
			bados <- 0
			# checking for missing ISO-codes
			count <- 0
			for (i in 1:length(ecs)) {
				if ( !is.na(ecs[i]) & !is.na(pos[i])  ) {
					if ( ecs[i] == "" & pos[i] != -2 ) {
						count <- count + 1
						bados <- bados + 1
						if ( count == 1 ) {
							cat("\n  For the following there is no ISO-code, so couldn't check the E classification.\n")
							cat("\nFor the following there is no ISO-code, so couldn't check the E classification.\n", file=issues_file, append=TRUE)
						}
						cat("    ", nas[i], "\n")
						cat("  ", nas[i], "\n", file=issues_file, append=TRUE)
					if ( pos[i] == -1 ) {
							cat("      Presumably it's because the doculect is extinct.\n")
							cat("    Presumably it's because the doculect is extinct.\n", file=issues_file, append=TRUE)
							bados <- bados - 1
						} else if ( get_wls_cls(firstlines[i]) == "Oth.SPEECH_REGISTER" ) {
							cat("      Presumably it's because the doculect is a speech register.\n")
							cat("    Presumably it's because the doculect is a speech register.\n", file=issues_file, append=TRUE)
							bados <- bados - 1
						} else if ( get_wls_cls(firstlines[i]) == "Oth.ARTIFICIAL" ) {
							cat("      Presumably it's because the doculect is artificial.\n")
							cat("    Presumably it's because the doculect is artificial.\n", file=issues_file, append=TRUE)
							bados <- bados - 1
						} else if ( get_wls_cls(firstlines[i]) == "Oth.FAKE" ) {
							cat("      Presumably it's because the doculect is fake.\n")
							cat("    Presumably it's because the doculect is fake.\n", file=issues_file, append=TRUE)
							bados <- bados - 1
						}
	
					}
				}
			}
			if ( optDATATYPE=="2" ) {
			# mention the ISO-codes we invented
				w_own_iso <- grep("0", iss)
				if ( length(w_own_iso) > 0 ) {
					cat("  For the following we devised our own ISO-code proxy:\n")
					print(nas[w_own_iso])
					cat("  Possibly some may have gotten ISO-codes meanwhile.\n")
					cat("  Maybe the classifications should also be updated.\n")
					cat("\nFor the following we devised our own ISO-code proxy:\n", file=issues_file, append=TRUE)
					cat(nas[w_own_iso], sep="\n", file=issues_file, append=TRUE)
					cat("Possibly some have gotten ISO-codes meanwhile.\n", file=issues_file, append=TRUE)
					cat("Maybe the classifications should also be updated.\n", file=issues_file, append=TRUE)
				}
			}
			# check for ISO-codes not in Ethnologue
			count <- count + 1
			for (i in 1:length(ecs)) {
				if ( !is.na(ecs[i]) ) {
					if ( ecs[i] != ""  & length(grep("0", iss)) == 0 ) {
						w_e <- which(ethn$ISO.639.3 == get_iso_code(secondlines[i]))[1]
						if ( length(w_e) == 0 ) {
							count <- count + 1
							if ( count == 1 ) {
								cat("\n", file=issues_file, append=TRUE)
							}
							cat("  The code", get_iso_code(secondlines[i]), "for", nas[i], "is not in Ethnologue\n")
							cat("The code", get_iso_code(secondlines[i]), "for", nas[i], "is not in Ethnologue\n", file=issues_file, append=TRUE)
							bados <- bados + 1
						}
					}
				}
			}
			# compare E classifications in ASJP and E
			cat("\n", file=issues_file, append=TRUE)
			count <- 0
			for (i in 1:length(ecs)) {
				if ( !is.na(ecs[i]) ) {
					if ( !is.na(iss[1]) & length(iss) != 1 ) {
						if ( ecs[i] != ""  & length(grep("0", ecs[i])) == 0 ) {
							w_e <- which(ethn$ISO.639.3 == get_iso_code(secondlines[i]))[1]
							if ( length(w_e) != 0 ) {
								ident <- identical(simplify_not_tolower(ethn$Classification[w_e]), get_e_cls(firstlines[i]))
								if ( ident==FALSE ) {
									count <- count + 1
									if ( count==1 ) {
										cat("  Cases where the Ethnologue classification in ASJP (given first)\n")
										cat("  differs from the one in Ethnologue (given second):\n\n")
										cat("\nCases where the Ethnologue classification in ASJP (given first)\n", file=issues_file, append=TRUE)
										cat("differs from the one in Ethnologue (given second):\n\n", file=issues_file, append=TRUE)
									}
									cat("\n", nas[i], "\n")
									cat("   ", get_e_cls(firstlines[i]), "\n")
									cat("   ", simplify_not_tolower(ethn$Classification[w_e]), "\n")
									cat("\n", nas[i], "\n", file=issues_file, append=TRUE)
									cat("   ", get_e_cls(firstlines[i]), "\n", file=issues_file, append=TRUE)
									cat("   ", simplify_not_tolower(ethn$Classification[w_e]), "\n", file=issues_file, append=TRUE)
									bados <- bados + 1
								}
							}
						}
					} else {
						bados <- bados + 1
						cat("  No E classification could be found.\n")
						cat("  Check if the second line is misaligned.\n")
						cat("  No E classification could be found.\n", file=issues_file, append=TRUE)
						cat("  Check if the second line is misaligned.\n", file=issues_file, append=TRUE)
					}
				}
			}
			w_na_e_cl <- which(is.na(ecs))
			if ( length(w_na_e_cl) > 0 ) {
				bados <- bados + 1
				for (k in 1:length(w_na_e_cl)) {
					cat("  For", get_name(firstlines[w_na_e_cl[k]]), "there is no E classification\n")
					cat("  For", get_name(firstlines[w_na_e_cl[k]]), "there is no E classification\n", file=issues_file, append=TRUE)
				}
			}
			if ( bados == 0 ) {
				cat("  The Ethnologue classification is ok.\n")
				cat("The Ethnologue classification is ok.\n", file=issues_file, append=TRUE)
			}
			cat("\nNow checking the Glottolog classification...\n")
			bados <- 0
			# checking for missing G classifications
			cat("\n", file=issues_file, append=TRUE)
			if ( length(which(is.na(gcs)))==length(gcs) ) {
				bados <- bados + 1
				cat("  The Glottolog classification is entirely missing.\n")
				cat("The Glottolog classification is entirely missing.\n", file=issues_file, append=TRUE)
			} else {
				if ( !exists("glot") ) {
					glot <<- read.table("glottolog.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
				}
				for (i in 1:length(firstlines)) {
					if ( !is.na(pos[i]) ) {
						if ( gcs[i] == "" & pos[i] != -2 ) {
							cat("sanity check\n")
							bados <- bados + 1
							cat("  For", nas[i], "there is no G classification.\n")
							cat("For", nas[i], "there is no G classification.\n", file=issues_file, append=TRUE)
							if ( pos[i] == -1 ) {
								cat("  --Presumably it's because the doculect is extinct.\n")
								cat("--Presumably it's because the doculect is extinct.\n", file=issues_file, append=TRUE)
								bados <- bados - 1
							} else if ( get_wls_cls(firstlines[i]) == "Oth.SPEECH_REGISTER" ) {
								cat("  --Presumably it's because the doculect is a speech register.\n")
								cat("--Presumably it's because the doculect is a speech register.\n", file=issues_file, append=TRUE)
								bados <- bados - 1
							} else if ( get_wls_cls(firstlines[i]) == "Oth.ARTIFICIAL" ) {
								cat("  --Presumably it's because the doculect is artificial.\n")
								cat("--Presumably it's because the doculect is artificial.\n", file=issues_file, append=TRUE)
								bados <- bados - 1
							} else if ( get_wls_cls(firstlines[i]) == "Oth.FAKE" ) {
								cat("  --Presumably it's because the doculect is fake.\n")
								cat("--Presumably it's because the doculect is fake.\n", file=issues_file, append=TRUE)
								bados <- bados - 1
							}
						}	
					}
				}
				count <- 0
				for (i in 1:length(secondlines)) {
					if ( !is.na(iss[i]) & length(grep(0, iss[i])) == 0 ) {
						w_ic <- which(glot[,1]==iss[i])
						if ( length(w_ic) > 0 ) {
							ident <- identical(gcs[i], simplify_not_tolower(glot[w_ic[1],2]))
							excused <- glot[w_ic,2]=="Unclassifiable" & gcs[i] == paste("Unclassifiable_", iss[i], sep="")
							if ( ident==FALSE & excused==FALSE ) {
								count <- count + 1
								if ( count==1 ) {
									cat("  Cases where the Glottolog classification in ASJP (given first)\n")
									cat("  differs from the one in Glottolog (given second):\n\n")
									cat("\nCases where the Glottolog classification in ASJP (given first)\n", file=issues_file, append=TRUE)
									cat("differs from the one in Glottolog (given second):\n\n", file=issues_file, append=TRUE)
								}
								cat("  ", paste(nas[i], " [", iss[i], "]", sep=""), "\n")
								cat("    ", gcs[i], "\n")
								cat("    ", glot[w_ic,2], "\n\n")
								cat(paste(nas[i], " [", iss[i], "]", sep=""), "\n", file=issues_file, append=TRUE)
								cat("  ", gcs[i], "\n", file=issues_file, append=TRUE)
								cat("  ", glot[w_ic,2], "\n\n", file=issues_file, append=TRUE)
								if ( length(grep("Spurious", glot[w_ic,2])) == 0 ) {
									bados <- bados + 1
								}
							}
						}
					}
				}
				w_u <- grep("Unclassifiable", gcs)
				if ( length(w_u) > 0 ) {
					count <- 0
					for (i in 1:length(w_u)) {
						if ( strsplit(gcs[w_u[i]], ",")[[1]][1] != paste("Unclassifiable_", iss[w_u[i]], sep="") ) {
							count <- count + 1
							bados <- bados + 1
							if ( count == 1 ) {
								cat("  For the following \"unclassifiables\" an underscore\n")
								cat("  plus the ISO-code should be appended to Unclassifiable:\n\n")
								cat("\nFor the following \"unclassifiables\" an underscore\n", file=issues_file, append=TRUE)
								cat("plus the ISO-code should be appended to Unclassifiable:\n\n", file=issues_file, append=TRUE)
							}
							cat("    ", nas[w_u[i]], paste("[", iss[w_u[i]], "]", sep=""), gcs[w_u[i]], "\n")
							cat("  ", nas[w_u[i]], paste("[", iss[w_u[i]], "]", sep=""), gcs[w_u[i]], "\n", file=issues_file, append=TRUE)
						}
					}
				}
			}
			if ( bados == 0 ) {
				cat("  The Glottolog classification is ok.\n")
				cat("The Glottolog classification is ok.\n", file=issues_file, append=TRUE)
			}
			# Update the Glottolog classification
			if ( bados  > 0 ) {
				cat("\nDo you want me to update the file\n")
				cat("with the latest Glottolog classification?\n")
				cat("\n   1. Yes.\n   2. No.\n\nType the number and press ENTER ")
				optUPDATEGLOT <- readline()
				optUPDATEGLOT <- fix_yn(optUPDATEGLOT); optUPDATEGLOT <- insist(optUPDATEGLOT, "12x"); regret(optUPDATEGLOT)
				if ( optUPDATEGLOT=="1" ) {
					revdata <- readLines(optFILEEDIT)
					slrev <- grep("\\{.*|.*@", revdata)
					for (i in 1:length(slrev)) {
						old_g_cls <- get_g_cls(revdata[slrev[i]])
						isorev <- get_iso_code(revdata[slrev[i]+1])
						w_isorev <- which(glot$ISO639.3==isorev)
						if ( length(w_isorev) > 0 ) {
							new_g_cls <- glot$Classification[w_isorev]
							if ( length(grep("Unattested,",new_g_cls))==1 ) {
								second <- strsplit(new_g_cls, "Unattested,")[[1]][2]
								new_g_cls <- paste("Unattested_", isorev, ",", second, sep="")
							}
							if ( new_g_cls=="Unattested" ) {
								new_g_cls <- paste("Unattested_", isorev, sep="")
							}
							if ( length(grep("Unclassifiable,",new_g_cls))==1 ) {
								second <- strsplit(new_g_cls, "Unclassifiable,")[[1]][2]
								new_g_cls <- paste("Unclassifiable_", isorev, ",", second, sep="")
							}
							if ( new_g_cls=="Unclassifiable" ) {
								new_g_cls <- paste("Unclassifiable_", isorev, sep="")
							}
							revdata[slrev[i]] <- substitute_g_classification(revdata[slrev[i]], new_g_cls)
						}
					}
					cat(revdata, file=optFILEEDIT, sep="\n")
					cat("\n", optFILEEDIT, "has been updated w.r.t. the Glottolog classification\n")
				}
			}
			cat("\nChecking the structure of the second line of metadata...\n")
			bados <- 0
			lengths <- as.vector(unlist(lapply(secondlines, nchar)))
			shortlines <- which(lengths < 42)
			cat("\n", file=issues_file, append=TRUE)
			if ( length(shortlines) > 0 ) {
				bados <- bados + 1
				for (i in 1:length(shortlines)) {
					cat("  The second line of metadata for", strsplit(firstlines[shortlines[i]], "\\{")[[1]][1], "is too short.\n")
					cat("The second line of metadata for", strsplit(firstlines[shortlines[i]], "\\{")[[1]][1], "is too short.\n", file=issues_file, append=TRUE)
				}
			}
			cat("\n", file=issues_file, append=TRUE)
			longlines <- which(lengths > 42)
			if ( length(longlines) > 0 ) {
				bados <- bados + 1
				for (i in 1:length(longlines)) {
					cat("  The second line of metadata for", strsplit(firstlines[longlines[i]], "\\{")[[1]][1], "is too long.\n")
					cat("The second line of metadata for", strsplit(firstlines[longlines[i]], "\\{")[[1]][1], "is too long.\n", file=issues_file, append=TRUE)
				}
			}
			# check that the numbers in col. 2 of the second line of metadata are 1-3
			cat("\n", file=issues_file, append=TRUE)
			levels <- as.vector(unlist(lapply(secondlines, classlevel)))
			not_1_to_3 <- which(levels %in% c("1", "2", "3")==FALSE)
			if ( length(not_1_to_3) > 0 ) {
				bados <- bados + 1
				for (i in 1:length(not_1_to_3)) {
					cat("  The second line of metadata for", strsplit(firstlines[not_1_to_3[i]], "\\{")[[1]][1], "does not have 1-3 in col. 2.\n")
					cat("The second line of metadata for", strsplit(firstlines[not_1_to_3[i]], "\\{")[[1]][1], "does not have 1-3 in col. 2.\n", file=issues_file, append=TRUE)
				}
			}
			if ( optDATATYPE == "2" ) {
				# check that the start of a family is indicated by 3
				w_mismatch <- w_start_fam[which(levels[w_start_fam]!="3")]
				if ( length(w_mismatch) > 0 ) {
					bados <- bados + 1
					for (i in 1:length(w_mismatch)) {
						cat("  For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where a family starts and no 3 in col. 2.\n")
						cat("For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where a family starts and no 3 in col. 2.\n", file=issues_file, append=TRUE)
					}
				}
				# check that the start of a genus is indicated by 3
				w_start_gen_not_fam <- setdiff(w_start_gen, w_start_fam)
				w_mismatch <- w_start_gen_not_fam[which(levels[w_start_gen_not_fam]!="2")]
				if ( length(w_mismatch) > 0 ) {
					cat("\n", file=issues_file, append=TRUE)
					bados <- bados + 1
					for (i in 1:length(w_mismatch)) {
						cat("  For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where a genus starts and no 2 in col. 2.\n")
						cat("For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where a genus starts and no 2 in col. 2.\n", file=issues_file, append=TRUE)
					}
				}
				w_no_start <- c(1:length(firstlines))[-c(w_start_fam, w_start_gen_not_fam)]
				w_mismatch <- w_no_start[which(levels[w_no_start]!="1")]
				if ( length(w_mismatch) > 0 ) {
					bados <- bados + 1
					cat("\n", file=issues_file, append=TRUE)
					for (i in 1:length(w_mismatch)) {
						cat("  For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where no family or genus starts and no 1 in col. 2.\n")
						cat("For", strsplit(firstlines[w_mismatch[i]], "\\{")[[1]][1], "there is a mismatch where no family or genus starts and no 1 in col. 2.\n", file=issues_file, append=TRUE)
					}
				}
			}
			# check that columns 1, 3, 11, 19, 31, 32, 33, 37, 38, 39 in the second line
			# of metadata are empty
			cat("\n", file=issues_file, append=TRUE)
			empty <- as.vector(unlist(lapply(secondlines, empty_columns)))
			not_10 <- which(empty!=10)
			if ( length(not_10) > 0 ) {
				bados <- bados + 1
				for (i in 1:length(not_10)) {
					cat("  The second line of metadata for", strsplit(firstlines[not_10[i]], "\\{")[[1]][1], "seems to be misaligned.\n")
					cat("The second line of metadata for", strsplit(firstlines[not_10[i]], "\\{")[[1]][1], "seems to be misaligned.\n", file=issues_file, append=TRUE)
				}
			}
			if ( bados==0 ) {
				cat("  The structure of the second line of metadata is ok.\n")
				cat("\nThe structure of the second line of metadata is ok.\n", file=issues_file, append=TRUE)
			}
			# checking coordinates
			cat("\nNow I will check the coordinates.\n")
			cat("  Do you want to\n")
			cat("    1. Go through with this step?\n")
			cat("    2. Skip it?\n")
			cat("Please type a number and press ENTER ")
			optCOORCHECK <- readline()
			optCOORCHECK <- fix_yn(optCOORCHECK); optCOORCHECK <- insist(optCOORCHECK, "12x"); regret(optCOORCHECK)
			# choice was to check coordinates
			if ( optCOORCHECK=="1" ) {
				sl <- scan("special_locations.txt", what="character", quiet=TRUE)
				dists <- lapply(secondlines, coor_ASJP)
				user_suppl <- grep("isocode_not_in_coor_file", dists)
				if ( length(user_suppl) > 0 ) {
					cat("  The coordinates for some doculects could not be checked.\n")
					cat("  See the file", issues_file, "for a list.\n")
					cat("\nThe coordinates for the following could not be checked:\n", file=issues_file, append=TRUE)
					for (i in 1:length(user_suppl)) {
						# cat("    ", strsplit(firstlines[user_suppl[i]], "\\{")[[1]][1], "\n")
						cat(strsplit(firstlines[user_suppl[i]], "\\{")[[1]][1], file=issues_file, append=TRUE)
						if ( i < length(user_suppl) ) {
							cat(", ", file=issues_file, append=TRUE)
						}
					}
					cat("\nFor all these doculects I will produce a map for sanity check\n")
					mapdata <- dists[user_suppl]
					la <- c(); lo <- c()
					for (i in 1:length(mapdata)) {
						la[i] <- as.vector(unlist(mapdata[[i]][3]))
						lo[i] <- as.vector(unlist(mapdata[[i]][4]))
					}
					nas <- unique(c(which(is.na(la)), which(is.na(lo))))
					if ( length(nas) > 0 ) {
						la <- la[-nas]
						lo <- lo[-nas]
					}
					simple_map(la,lo)
				}
				# now get distances that are more than 100 km from Ethnologue's distances
				bados <- 0
				count <- 0
				relevant <- setdiff(c(1:length(dists)), user_suppl)
				if ( length(relevant) > 0 ) {
					ds <- c()
					where <- c()
					count2 <- 0
					for (i in 1:length(relevant)) {
						count <- count + 1
						ds[count] <- as.vector(unlist(dists[[relevant[i]]][2]))
						where[count] <- relevant[i]
					}
					notaps <- which(is.na(ds))
					if ( length(notaps) > 0 ) {
						ds <- ds[-notaps]
						where <- where[-notaps]
					}
					if ( length(ds) > 0 ) {
						for (i in 1:length(ds)) {
							if ( ds[i] > 100 & strsplit(firstlines[where[i]], "\\{")[[1]][1] %in% sl == FALSE ) {
								count2 <- count2 + 1
								bados <- bados + 1
								if ( count2 == 1 ) {
									cat("\nHere follows a list of all the doculects for which\n")
									cat("the coordinates differ by more than 100 km from Ethnologue.\n")
									cat("There will be a distance and a doculect name.\n\n")
									cat("\n\nHere follows a list of all the doculects for which the coordinates differ by more than 30 km from Ethnologue\n", file=issues_file, append=TRUE)
									cat("There will be a distance and a doculect name.\n\n", file=issues_file, append=TRUE)
								}
								cat("  ", round(ds[i]), "\t", strsplit(firstlines[where[i]], "\\{")[[1]][1], "\n")
								cat(round(ds[i]), "\t", strsplit(firstlines[where[i]], "\\{")[[1]][1], "\n", file=issues_file, append=TRUE)
							}
						}
					}
				}
				if ( bados==0 ) {
					cat("  The coordinates are ok.\n")
					cat("\nThe coordinates are ok.\n", file=issues_file, append=TRUE)
				}
			}
			cat("\nChecking population figures...\n")
			pops <- suppressWarnings(lapply(secondlines, pop_ASJP))
			if ( length(secondlines) > 0 ) {
				if ( length(pops) > 0 ) {
					count <- 0
					for (i in 1:length(secondlines)) {
						if ( length(grep("e", (strsplit(secondlines[i], "")[[1]][20:30]))) > 0 ) {
							scinot <- trimws(paste(strsplit(secondlines[i], "")[[1]][20:30], collapse=""))
							count <- count + 1
							if ( count == 1 ) {
							cat("\nCases where the population figures use scientific notation:\n")
								cat("  Doculect\tPopulation\n")
								cat("\nCases where the population figures use scientific notation:\n", file=issues_file, append=TRUE)
								cat("Doculect\tPopulation\n", file=issues_file, append=TRUE)
							}
							cat("  ", strsplit(firstlines[i], "\\{")[[1]][1], "\t", scinot, "\n")
							cat(strsplit(firstlines[i], "\\{")[[1]][1], "\t", scinot, "\n", file=issues_file, append=TRUE)
						}
					}
					count <- 0
					for (i in 1:length(pops)) {
						if ( !is.na(pops[[i]][[1]][1]) & !is.na(pops[[i]][[2]][1]) ) {
							if ( pops[[i]][[1]][1] != pops[[i]][[2]][1] ) {
								count <- count + 1
								if ( count == 1 ) {
								cat("\nCases where the population figures in Ethnologue are different from ASJP:\n")
									cat("  Doculect\tEthno_pop\tASJP_pop\n")
									cat("\nCases where the population figures in Ethnologue are different from ASJP:\n", file=issues_file, append=TRUE)
									cat("Doculect\tEthno_pop\tASJP_pop\n", file=issues_file, append=TRUE)
								}
								cat("  ", strsplit(firstlines[i], "\\{")[[1]][1], "\t", pops[[i]][[1]][1], "\t", pops[[i]][[2]][1], "\n")
								cat(strsplit(firstlines[i], "\\{")[[1]][1], "\t", pops[[i]][[1]][1], "\t", pops[[i]][[2]][1], "\n", file=issues_file, append=TRUE)
							}
						}
					}
					if ( count == 0 ) {
						cat("  The population figures are ok.\n")
						cat("\nThe population figures are ok.\n", file=issues_file, append=TRUE)
					} else {
						# Update population figures
						cat("\nDo you want me to update the file\n")
						cat("with the most recent populations figures?\n")
						cat("\n   1. Yes.\n   2. No.\n\nType the number and press ENTER ")
						optUPDATEPOP <- readline()
						optUPDATEPOP <- fix_yn(optUPDATEPOP); optUPDATEPOP <- insist(optUPDATEPOP, "12x"); regret(optUPDATEPOP)
						if ( optUPDATEPOP=="1" ) {
							revdata <- readLines(optFILEEDIT)
							slrev <- grep("\\{.*|.*@", revdata) + 1
							for (i in 1:length(slrev)) {
								oldpop <- get_population(revdata[slrev[i]])
								if ( oldpop >= 0 ) {
									isorev <- get_iso_code(revdata[slrev[i]])
									if ( !is.na(isorev) & length(grep("0", isorev))==0 ) {
										w_isorev <- which(ethn$ISO.639.3==isorev)
										if ( length(w_isorev) > 0 ) {
											newpop <- as.numeric(ethn$Population.Numeric[w_isorev])
											if ( (!is.na(newpop) & newpop!=oldpop) & newpop!=0 ) {
											 	revdata[slrev[i]] <- substitute_population(revdata[slrev[i]], oldpop, newpop)
											}
											if ( (!is.na(newpop) & newpop!=oldpop) & (newpop==0 & oldpop >= 0) ) {
											 	revdata[slrev[i]] <- substitute_population(revdata[slrev[i]], oldpop, -1)
												cat("  For", get_name(revdata[slrev[i]-1]), "put in year of extinction\n")
												cat("For", get_name(revdata[slrev[i]-1]), "put in year of extinction\n", file=issues_file, append=TRUE)
											}
										}
									}
								}
							}
							cat(revdata, file=optFILEEDIT, sep="\n")
							cat("\n", optFILEEDIT, "has been updated w.r.t. population figures\n")
						}
					}
				} else {
					cat("  Could not find a population figure.\n")
					cat("\nCould not find a population figure.\n", file=issues_file, append=TRUE)
				}
			}
			# now checking WALS code
			bados <- 0
			cat("\nChecking the WALS code...\n")
			if ( !exists("wals") ) {
				wals <<- read.table("wals_iso_mappings.txt", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="")
			}
			nas1 <- unique(c(which(is.na(wals[,1])), which(is.na(wals[,2]))))
			if ( length(nas1) > 0 ) {
				wals_red <- wals[-nas1,]
			} else {
				wals_red <- wals
			}
			pairs_correct <- unique(paste(wals_red[,1], "_", wals_red[,2], sep=""))
			nas2 <- unique(c(which(is.na(was)), which(is.na(iss))))
			pairs_ASJP <- paste(was, "_", iss, sep="")
			if ( length(nas2) > 0 ) {
				pairs_ASJP <- pairs_ASJP[-nas2]
			}
			wrong <- setdiff(pairs_ASJP, pairs_correct)
			count <- 0
			cat("\n", file=issues_file, append=TRUE)
			for (i in 1:length(secondlines)) {
				wc <- get_wls_code(secondlines[i])
				ic <- get_iso_code(secondlines[i])
				wc_ic <- paste(wc, "_", ic, sep="")
				if ( wc_ic %in% wrong ) {
					bados <- bados + 1
					count <- count + 1
					if ( count == 1 ) {
						cat("    The following appear to have the wrong combination of WALS-code and ISO-code:\n")
						cat("    WALS\tISO\tNAME\n")
						cat("The following appear to have the wrong combination of WALS-code and ISO-code:\n", file=issues_file, append=TRUE)
						cat("  WALS\tISO\tNAME\n", file=issues_file, append=TRUE)
					}
					cat("   ", wc, "\t", ic, "\t", get_doculect(firstlines[i]), "\n")
					cat(" ", wc, "\t", ic, "\t", get_doculect(firstlines[i]), "\n", file=issues_file, append=TRUE)
				}
			}
			# this is specially for single word lists
			if ( length(secondlines) == 1 & is.na(wc) ) {
				cat("No WALS code could be found.\n")
				cat("\nNo WALS code could be found.\n", file=issues_file, append=TRUE)
				bados <- bados + 1
			}
			if ( bados==0 ) {
				cat("  No issues found with the WALS code.\n")
				cat("\nNo issues found with the WALS code.\n", file=issues_file, append=TRUE)
			}

			# checking data lines
			if ( optCHECKDATA=="1" ) {
				bados <- 0
				count <- 0
				for (i in 1:length(datalines)) {
					before_slashes <- strsplit(cdata[datalines[i]], "//")[[1]][1]
					if ( length(grep("[[:digit:]]{,3} [[:alpha:]]*\t[%Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!ieE3auo ,\\*\\$\"\\~]*", cdata[datalines[i]])) == 0 | length(grep("  ", before_slashes)) > 0 ) {
						count <- count + 1
						if ( count == 1 ) {
							bados <- bados + 1
							cat("  The following line(s) don't follow the general format:\n")
							cat("\nThe following line(s) don't follow the general format:\n", file=issues_file, append=TRUE)
						}
						print(cdata[datalines[i]])
						cat(cdata[datalines[i]], "\n", file=issues_file, append=TRUE)
					}
				}
				XXX <- grep("XXX", cdata[datalines])
				w_bad_xxx <- c()
				for (i in 1:length(XXX)) {
					s1 <- strsplit(cdata[datalines][XXX[i]], "\\\t")[[1]][2]
					s2 <- strsplit(s1, " /")[[1]][1]
					if ( s2 != "XXX" ) {
						bados <- bados + 1
						w_bad_xxx <- c(w_bad_xxx, i)
					}
				}
				if ( length(w_bad_xxx) > 0 ) {
					cat("  The following line(s) have XXX and something else in them\n")
					cat("\nThe following line(s) have XXX and something else in them\n", file=issues_file, append=TRUE)
					cat(cdata[datalines][XXX[w_bad_xxx]], sep="\n")
					cat(cdata[datalines][XXX[w_bad_xxx]], sep="\n", file=issues_file, append=TRUE)
				}
				w_XX <- grep("\tXX ", cdata[datalines])
				if ( length(w_XX) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have XX in them\n")
					cat("\nThe following line(s) have XX in them\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[w_XX]], sep="\n")
					cat(cdata[datalines[w_XX]], sep="\n", file=issues_file, append=TRUE)
				}
				w_slash_no_space <- grep("[Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!ieE3auo,\\*\\$\"\\~]//", cdata[datalines])
				if ( length(w_slash_no_space) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have no space before //\n")
					cat("\nThe following line(s) have no space before //\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[w_slash_no_space]], sep="\n")
					cat(cdata[datalines[w_slash_no_space]], sep="\n", file=issues_file, append=TRUE)
				}
				# allascii <- rawToChar(as.raw(32:126),multiple=TRUE)
				# ASJPcode <- strsplit("Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!ieE3auo,*$\"~ ", "")[[1]]
				# nonASJPcode <- setdiff(allascii, ASJPcode)
				w_non_ASJPcode <- grep("\\\t.*[#&\\'()+\\./01269:;<=>?@ABDFHIJKMOPQRUVWY^_`\\{\\|\\{].*//", cdata[datalines])
				if ( length(w_non_ASJPcode) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) contain a non-ASJPcode character\n")
					cat("\nThe following line(s) contain a non-ASJPcode character\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[w_non_ASJPcode]], sep="\n")
					cat(cdata[datalines[w_non_ASJPcode]], sep="\n", file=issues_file, append=TRUE)
				}
				tab_space <- grep("\\\t ", cdata[datalines])
				if ( length(tab_space) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have a tab followed by space\n")
					cat("\nThe following line(s) have a tab followed by space\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[tab_space]], sep="\n")
					cat(cdata[datalines[tab_space]], sep="\n", file=issues_file, append=TRUE)
				}
				space_comma <- grep(" ,", cdata[datalines])
				if ( length(space_comma) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have a space before a comma\n")
					cat("\nThe following line(s) have a space before a comma\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[space_comma]], sep="\n")
					cat(cdata[datalines[space_comma]], sep="\n", file=issues_file, append=TRUE)
				}
				comma_no_space <- grep(",[Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!ieE3auo,\\*\\$\"\\~]", cdata[datalines])
				if ( length(comma_no_space) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have a comma not followed by a space\n")
					cat("\nThe following line(s) have a comma not followed by a space\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[comma_no_space]], sep="\n")
					cat(cdata[datalines[comma_no_space]], sep="\n", file=issues_file, append=TRUE)
				}
				asteriks_after_consonant <- grep(",[Xpbfvm8tdszcnSZCTkgxqXL4G!\\*\"\\~]\\*", cdata[datalines])
				if ( length(asteriks_after_consonant) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have an illegitimate placement of the asteriks\n")
					cat("\nThe following line(s) have an illegitimate placement of the asteriks\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[asteriks_after_consonant]], sep="\n")
					cat(cdata[datalines[asteriks_after_consonant]], sep="\n", file=issues_file, append=TRUE)
				}
				comma_no_word <- grep(", *//", cdata[datalines])
				if ( length(comma_no_word) > 0 ) {
					bados <- bados + 1
					cat("  The following line(s) have a comma followed by nothing\n")
					cat("\nThe following line(s) have a comma followed by nothing\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[comma_no_word]], sep="\n")
					cat(cdata[datalines[comma_no_word]], sep="\n", file=issues_file, append=TRUE)
				}
				no_slashes <- setdiff(1:length(datalines), grep("//", cdata[datalines]))
				if ( length(no_slashes) > 0 ) {
					bados <- bados + 1
					cat("  The following data line(s) are missing //\n")
					cat("\nThe following data line(s) are missing //\n", file=issues_file, append=TRUE)
					cat(cdata[datalines[no_slashes]], sep="\n")
					cat(cdata[datalines[no_slashes]], sep="\n", file=issues_file, append=TRUE)
				}
				quote_after_vowel <- grep("[ieE3auo]\"", cdata[datalines])
				count <- 0
				if ( length(quote_after_vowel) > 0 ) {
					for (i in 1:length(cdata[datalines[quote_after_vowel]])) {
						before_slash <- strsplit(cdata[datalines[quote_after_vowel]][i], "//")[[1]][1]
						if ( length(grep("[ieE3auo]\"", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have an illegitimate placement of the quotation mark\n")
								cat("\nThe following line(s) have an illegitimate placement of the quotation mark\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[quote_after_vowel]][i], "\n")
							cat(cdata[datalines[quote_after_vowel]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				tilde_after_vowel <- grep("[ieE3auo]\\~", cdata[datalines])
				count <- 0
				if ( length(tilde_after_vowel) > 0 ) {
					for (i in 1:length(cdata[datalines[tilde_after_vowel]])) {
						before_slash <- strsplit(cdata[datalines[tilde_after_vowel]][i], "//")[[1]][1]
						if ( length(grep("[ieE3auo]\\~", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have an illegitimate placement of the tilde\n")
								cat("\nThe following line(s) have an illegitimate placement of the tilde\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[tilde_after_vowel]][i], "\n")
							cat(cdata[datalines[tilde_after_vowel]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				tilde_after_space_C <- grep(" [Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!]~", cdata[datalines])
				tilde_after_tab_C <- grep("\\\t[Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!]~", cdata[datalines])
				tilde_after_C <- union(tilde_after_space_C, tilde_after_tab_C)
				count <- 0
				if ( length(tilde_after_C) > 0 ) {
					for (i in 1:length(cdata[datalines[tilde_after_C]])) {
						before_slash <- strsplit(cdata[datalines[tilde_after_C]][i], "//")[[1]][1]
						if ( length(grep(" [Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!]~", before_slash)) == 1 | length(grep("\\\t[Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!]~", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have a tilde modifying a single consonant\n")
								cat("\nThe following line(s) have a tilde modifying a single consonant\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[tilde_after_C]][i], "\n")
							cat(cdata[datalines[tilde_after_C]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				tilde_after_VC <- grep("[ieE3auo][Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!\"\\~]\\~", cdata[datalines])
				count <- 0
				if ( length(tilde_after_VC) > 0 ) {
					for (i in 1:length(cdata[datalines[tilde_after_VC]])) {
						before_slash <- strsplit(cdata[datalines[tilde_after_VC]][i], "//")[[1]][1]
						if ( length(grep("[ieE3auo][Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!\"\\~]\\~", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have an illegitimate placement of the tilde\n")
								cat("\nThe following line(s) have an illegitimate placement of the tilde\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[tilde_after_VC]][i], "\n")
							cat(cdata[datalines[tilde_after_VC]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				dollar_after_vowel <- grep("[ieE3auo]\\$", cdata[datalines])
				count <- 0
				if ( length(dollar_after_vowel) > 0 ) {
					for (i in 1:length(cdata[datalines[dollar_after_vowel]])) {
						before_slash <- strsplit(cdata[datalines[dollar_after_vowel]][i], "//")[[1]][1]
						if ( length(grep("[ieE3auo]\\$", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have an illegitimate placement of the dollar sign\n")
								cat("\nThe following line(s) have an illegitimate placement of the dollar sign\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[dollar_after_vowel]][i], "\n")
							cat(cdata[datalines[dollar_after_vowel]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				dollar_after_VC <- grep("[ieE3auo][Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!\"\\~]\\$", cdata[datalines])
				count <- 0
				if ( length(dollar_after_VC) > 0 ) {
					for (i in 1:length(cdata[datalines[dollar_after_VC]])) {
						before_slash <- strsplit(cdata[datalines[dollar_after_VC]][i], "//")[[1]][1]
						if ( length(grep("[ieE3auo][Xpbfvmw8tdszcnrlSZCjT5ykgxNqXh7L4G!\"\\~]\\$", before_slash)) == 1 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have an illegitimate placement of the dollar sign\n")
								cat("\nThe following line(s) have an illegitimate placement of the dollar sign\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[dollar_after_VC]][i], "\n")
							cat(cdata[datalines[dollar_after_VC]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				synonyms <- grep(",", cdata[datalines])
				count <- 0
				if ( length(synonyms) > 0 ) {
					for (i in 1:length(cdata[datalines[synonyms]])) {
						a1 <- strsplit(cdata[datalines[synonyms]][i], "\t")[[1]][2]
						a2 <- strsplit(a1, " //")[[1]][1]
						a3 <- strsplit(a2, ", ")[[1]]
						if ( anyDuplicated(a3) > 0 ) {
							count <- count + 1
							bados <- bados + 1
							if ( count==1 ) {
								cat("  The following line(s) have synonyms of the same shape\n")
								cat("\nThe following line(s) have synonyms of the same shape\n", file=issues_file, append=TRUE)
							}
							cat(cdata[datalines[synonyms]][i], "\n")
							cat(cdata[datalines[synonyms]][i], "\n", file=issues_file, append=TRUE)
						}
					}
				}
				if ( bados == 0 ) {
					cat("  No issues were found with the datalines.\n")
					cat("\nNo issues were found with the datalines.\n", file=issues_file, append=TRUE)
				}
			}
		}
		# choice was to get statistics
		if ( length(grep("2", optEDITOR))==1 ) {
			stats_file <- paste("stats_", optFILEEDIT, sep="")
			cat("Statistics will be recorded in a file called", stats_file, "\n")
			cat("\n  I can include a count of words, but this may take around a minute.\n")
			cat("  Do you want to\n")
			cat("    1. Go through with this step?\n")
			cat("    2. Skip it?\n\n")
			cat("Please type a number and press ENTER ")
			optWORDCOUNT <- readline()
			optWORDCOUNT <- fix_yn(optWORDCOUNT); optWORDCOUNT <- insist(optWORDCOUNT, "12x"); regret(optWORDCOUNT)

			# language coverage
			cat("STATISTICS\n")
			cat("STATISTICS\n", file=stats_file)
			all_isos <- unique(as.vector(na.omit(iss)))
			number_all_isos <- length(all_isos)
			own_isos <- grep(0, all_isos)
			number_own_isos <- length(own_isos) 
			number_isos <- number_all_isos - number_own_isos
			cat("Distinct ISO 639-3 languages:\t", number_isos, "\n")
			cat("Distinct ISO 639-3 languages:\t", number_isos, "\n", file=stats_file, append=TRUE)
			cat("Additional languages that don't have ISO 639-3 codes but should:\t", length(own_isos), "\n")
			cat("Additional languages that don't have ISO 639-3 but should:\t", length(own_isos), "\n", file=stats_file, append=TRUE)
			sign_in_e <- length(grep("Sign language", ethn$Classification))
			number_isos_in_e <- length(unique(ethn$ISO.639.3)) - sign_in_e
			isos_in_asjp_not_in_e <- iss[which(is.na(match(iss, unique(ethn$ISO.639.3))))]
			isos_in_asjp_not_in_e <- as.vector(na.omit(isos_in_asjp_not_in_e))
			if ( length(grep("0", isos_in_asjp_not_in_e)) > 0 ) {
				isos_in_asjp_not_in_e <- isos_in_asjp_not_in_e[-grep("0", isos_in_asjp_not_in_e)]
			}
			if ( length(grep("8", isos_in_asjp_not_in_e)) > 0 ) {
				isos_in_asjp_not_in_e <- isos_in_asjp_not_in_e[-grep("8", isos_in_asjp_not_in_e)]
			}
			isos_in_asjp_not_in_e <- unique(isos_in_asjp_not_in_e)
			number_missing_isos <- number_isos_in_e - number_isos + length(isos_in_asjp_not_in_e)
			cat("Missing ISO 639-3 languages in Ethnologue (ignoring sign languages):\t", number_missing_isos, "\n")
			cat("Missing ISO 639-3 languages in Ethnologue  (ignoring sign languages):\t", number_missing_isos, "\n", file=stats_file, append=TRUE)
			language_coverage <- round(100 * (number_isos - length(isos_in_asjp_not_in_e)) / number_isos_in_e, 2)
			cat("Language coverage:\t", language_coverage, "percent\n")
			cat("Language coverage:\t", language_coverage, "percent\n", file=stats_file, append=TRUE)

			# count types of doculect
			cat("Doculects:\t", length(firstlines), "\n")
			cat("Doculects:\t", length(firstlines), "\n", file=stats_file, append=TRUE)
			no_protos <- length(grep("PROTO_", firstlines))
			cat("Proto-languages:\t", no_protos, "\n")
			cat("Proto-languages:\t", no_protos, "\n", file=stats_file, append=TRUE)
			cat("Ancient languages:\t", length(which(pos=="-2")) - no_protos, "\n")
			cat("Ancient languages:\t", length(which(pos=="-2")) - no_protos, "\n", file=stats_file, append=TRUE)
			cat("Recently extinct languages:\t", length(which(pos=="-1")) + length(which(as.numeric(pos) < -3)), "\n")
			cat("Recently extinct languages:\t", length(which(pos=="-1")) + length(which(as.numeric(pos) < -3)), "\n", file=stats_file, append=TRUE)
			cat("Constructed languages:\t", length(grep("Oth.ARTIFICIAL", wls_gen_all)), "\n")
			cat("Constructed languages:\t", length(grep("Oth.ARTIFICIAL", wls_gen_all)), "\n", file=stats_file, append=TRUE)
			# number of 40-item and 100-item lists
			nextlist <- w_first[-1]
			thislist <- w_first[-length(w_first)]
			lengths <- (nextlist - thislist) - 2
			hundred_item <- length(which(lengths > 42))
			if ( length(w_first[length(w_first)]:last_data_line) - 2 > 40 ) {
				hundred_item <- hundred_item + 1
			}
			cat("100-item lists:\t", hundred_item, "\n")
			cat("100-item lists:\t", hundred_item, "\n", file=stats_file, append=TRUE)

			# G families in ASJP 
			if ( !exists("glot") ) {
				glot <<- read.table("glottolog.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
			}
			g_fams_in_asjp <- get_fams(gcs)
			if ( length(which(g_fams_in_asjp == "")) > 0 ) {
				g_fams_in_asjp <- g_fams_in_asjp[-which(g_fams_in_asjp == "")]
			}
			if ( length(which(g_fams_in_asjp == "NA")) > 0 ) {
				g_fams_in_asjp <- g_fams_in_asjp[-which(g_fams_in_asjp == "NA")]
			}
			# to be subtracted: ArtificialLanguage, MixedLanguage, Pidgin
			# Spurious, SpeechRegister
			cat("Glottolog families (including some not in Glottolog):\t", length(g_fams_in_asjp), "\n")
			cat("Glottolog families (including some not in Glottolog):\t", length(g_fams_in_asjp), "\n", file=stats_file, append=TRUE)
			g_fams <- get_fams(glot[,2])
			missing_g_fams <- setdiff(g_fams, g_fams_in_asjp)
			if ( "SignLanguage" %in% missing_g_fams ) {
				missing_g_fams <- missing_g_fams[-match("SignLanguage", missing_g_fams)]
			}
			if ( "Unattested" %in% missing_g_fams ) {
				missing_g_fams <- missing_g_fams[-match("Unattested", missing_g_fams)]
			}
			if ( "Unclassifiable" %in% missing_g_fams ) {
				missing_g_fams <- missing_g_fams[-match("Unclassifiable", missing_g_fams)]
			}
			cat("Missing Glottolog families:\t", length(missing_g_fams), "\n")
			cat("Missing Glottolog families:\t", length(missing_g_fams), "\n", file=stats_file, append=TRUE)
			uc <- grep("Unclassifiable", glot[,2])
			uc_designations <- glot[uc,1]
			uc_with_isocode <- uc_designations[-grep("NOCODE_", uc_designations)]
			uc_without_isocode <- uc_designations[grep("NOCODE_", uc_designations)]
			uc_with_isocode_not_in_asjp <- uc_with_isocode[which(is.na(match(uc_with_isocode, iss)))]
			cat("Glottolog Unclassifiables with ISO-codes not in ASJP:\t", length(uc_with_isocode_not_in_asjp), "\n")
			cat("Glottolog Unclassifiables with ISO-codes not in ASJP:\t", length(uc_with_isocode_not_in_asjp), "\n", file=stats_file, append=TRUE)
			delete_nocode <- function(x) strsplit(x, "NOCODE_")[[1]][2]
			names_uc_without_isocode <- as.vector(unlist(lapply(uc_without_isocode, delete_nocode)))
			names_uc_without_isocode <- as.vector(unlist(lapply(names_uc_without_isocode, toupper)))
			matching_names <- match(names_uc_without_isocode, nas)
			names_uc_without_isocode_not_in_asjp <- names_uc_without_isocode[which(is.na(matching_names))]
			names_uc_without_isocode_not_in_asjp <- as.vector(unlist(lapply(names_uc_without_isocode_not_in_asjp, tolower)))
			names_uc_without_isocode_not_in_asjp <- as.vector(unlist(lapply(names_uc_without_isocode_not_in_asjp, capitalize)))
			cat("Glottolog Unclassifiables without ISO-codes apparently not in ASJP:\t", length(names_uc_without_isocode_not_in_asjp), "\n")
			cat("Glottolog Unclassifiables without ISO-codes apparently not in ASJP:\t", length(names_uc_without_isocode_not_in_asjp), "\n", file=stats_file, append=TRUE)

			# E families in ASJP 
			# pidgins, creoles, and constructed
			# make up 3 "families", which should be subtracted
			e_fams_in_asjp <- get_fams(ecs)
			if ( length(which(e_fams_in_asjp == "")) > 0 ) {
				e_fams_in_asjp <- e_fams_in_asjp[-which(e_fams_in_asjp == "")]
			}
			delete_curly_plus <- function(x) {
				x <- strsplit(x, "\\}")[[1]][1]
			}
			e_fams_in_asjp <- unique(as.vector(unlist(lapply(e_fams_in_asjp, delete_curly_plus))))
			where_empty <- which(e_fams_in_asjp=="")
			if ( length(where_empty) > 0 ) {
				e_fams_in_asjp <- e_fams_in_asjp[-where_empty]
			}
			where_na <- which(is.na(e_fams_in_asjp))
			if ( length(where_na) > 0 ) {
				e_fams_in_asjp <- e_fams_in_asjp[-where_na]
			}
			number_e_fams_in_asjp <- length(e_fams_in_asjp)
			cat("Ethnologue families and isolates:\t", number_e_fams_in_asjp, "\n")
			cat("Ethnologue families and isolates:\t", number_e_fams_in_asjp, "\n", file=stats_file, append=TRUE)
			fams_in_e <- get_fams(ethn$Classification)
			unclassified_in_e <- length(which(ethn$Classification=="Unclassified"))
			isolates_in_e <- length(which(ethn$Classification=="Language isolate"))
			# get the number of families in Ethnologue 
			# pidgins, creoles, sign lgs, constructed, unclassified, and isolates
			# make up 6 "families", which should be subtracted
			# and the numbers of unclassified and isolates should be added
			number_e_isolates_in_asjp <- length(grep("Languageisolat", e_fams_in_asjp))
			number_e_uc_in_asjp <- length(grep("Unclassified", e_fams_in_asjp))
			number_e_isolates_or_uc_in_asjp <- number_e_isolates_in_asjp + number_e_uc_in_asjp
			cat("Ethnologue isolates:\t", number_e_isolates_or_uc_in_asjp, "\n")
			cat("Ethnologue isolates:\t", number_e_isolates_or_uc_in_asjp, "\n", file=stats_file, append=TRUE)
			missing_e_isolates_or_uc <- unclassified_in_e + isolates_in_e - number_e_isolates_or_uc_in_asjp
			cat("Missing Ethnologue isolates:\t", missing_e_isolates_or_uc, "\n")
			cat("Missing Ethnologue isolates:\t", missing_e_isolates_or_uc, "\n", file=stats_file, append=TRUE)

			# WALS families and languages in ASJP
			cat("WALS families (including some not in WALS):\t", length(wls_fam_all_unique), "\n")
			cat("WALS families (including some not in WALS):\t", length(wls_fam_all_unique), "\n", file=stats_file, append=TRUE)
			was_not_na <- unique(was[!is.na(was)])
			cat("Languages also in WALS:\t", length(was_not_na), "\n")
			cat("Languages also in WALS:\t", length(was_not_na), "\n", file=stats_file, append=TRUE)
			# count entries and words
			synsets <- as.vector(unlist(lapply(cdata[datalines], get_synset)))
			w_missing <- grep("XXX", synsets)
			if ( length(w_missing) > 0 ) {
				synsets <- synsets[-w_missing]
			}
			cat("Entries (synsets):\t", length(synsets), "\n")
			cat("Entries (synsets):\t", length(synsets), "\n", file=stats_file, append=TRUE)
			# option was to also count the number of words
			if ( optWORDCOUNT=="1" ) {
				no_words <- as.vector(unlist(lapply(synsets, count_words)))
				cat("Words:\t", sum(no_words), "\n")
				cat("Words:\t", sum(no_words), "\n", file=stats_file, append=TRUE)
			}
			cat("\nTO-DO\n")
			cat("\nTO-DO\n", file=stats_file, append=TRUE)
			cat("Missing Glottolog families:\n")
			cat("Missing Glottolog families:\n", file=stats_file, append=TRUE)
			print(missing_g_fams)
			cat(missing_g_fams, file=stats_file, sep=", ", append=TRUE)
			w_sign <- grep("Sign language", ethn$Classification)
			cat("\nGlottolog Unclassifiables with ISO-codes not in ASJP:\n")
			cat("\nGlottolog Unclassifiables with ISO-codes not in ASJP:\n", file=stats_file, append=TRUE)
			print(sort(uc_with_isocode_not_in_asjp))
			cat(sort(uc_with_isocode_not_in_asjp), sep=", ", file=stats_file, append=TRUE)
			cat("\nGlottolog Unclassifiables without ISO-codes apparently not in ASJP:\n")
			print(sort(names_uc_without_isocode_not_in_asjp))
			cat("\nGlottolog Unclassifiables without ISO-codes apparently not in ASJP:\n", file=stats_file, append=TRUE)
			cat(sort(names_uc_without_isocode_not_in_asjp), sep=", ", file=stats_file, append=TRUE)
			e_isos_no_sign <- ethn$ISO.639.3[-w_sign]
			e_names_no_sign <- ethn$name[-w_sign]
			match_e_isos_asjp_isos <- match(e_isos_no_sign, unique(iss))
			missing_isos <- which(is.na(match_e_isos_asjp_isos))
			output_isos <- e_isos_no_sign[missing_isos]
			output_names <- e_names_no_sign[missing_isos]
			mapfile_prefix <- strsplit(optFILEEDIT, "\\.")[[1]][1]
			mapfile <- paste("missing_", mapfile_prefix, ".pdf", sep="")
			missing_file <- paste("missing_", optFILEEDIT, sep="")
			cat("name\tISO\tGlottolog_classification\n", file=missing_file)
			for (i in 1:length(output_isos)) {
				w_iso <- which(glot$ISO639.3==output_isos[i])
				if ( length(w_iso) > 0 ) {
					glot_clas <- glot$Classification[w_iso]
				} else {
					glot_clas <- "not_in_Glottolog"
				}
				cat(output_names[i], "\t", output_isos[i], "\t", glot_clas, "\n", sep="", file=missing_file, append=TRUE)
			}
			## make a map of the missing languages
			# cat("\nClick away or save the map of missing languages\n")
			# glocs <- read.table(file="e_locs.txt", header=FALSE, stringsAsFactors=FALSE)
			# m_glocs <- match(output_isos, glocs[,1])
			# m_glocs <- m_glocs[!is.na(m_glocs)]
			# mis_lats <- glocs[,2][m_glocs]
			# mis_lons <- glocs[,3][m_glocs]
			# simple_map(mis_lats, mis_lons)
			# # pdf(mapfile)
			# # png(mapfile)
			# # dev.off()
			cat("For missing ISO-639-9 languages see\n")
			cat("   ", missing_file, "\n")
			# cat("   ", missing_file, "and", mapfile, "\n")
			## get the number of missing items
			# function for counting attestations of 40-item members in a word list (wl)
			att <- function(wl) {
				attestations <- 0
				forties <- c("1",  "2",  "3",  "11",  "12",  "18",  "19",  "21",  "22",  "23",  "25",  "28",  "30",  "31",  "34",  "39",  "40",  "41",  "43",  "44",  "47",  "48",  "51",  "53",  "54",  "57",  "58",  "61",  "66",  "72",  "74",  "75",  "77",  "82",  "85",  "86",  "92",  "95",  "96",  "100")
				for (w in 1:length(wl)) {
					id <- strsplit(wl[w], " ")[[1]][1]
					da <- strsplit(wl[w], "\\\t")[[1]][2]
					if ( id %in% forties & length(grep("XXX", da))==0 ) {
						attestations <- attestations + 1
					}
				}
				return(attestations)
			}
			list_names <- c()
			list_isos <- c()
			list_attestations <- c()
			for (i in 1:length(thislist)) {
				wordlist <- cdata[(thislist[i] + 2):(nextlist[i] - 1)]
				list_names[i] <- strsplit(cdata[thislist[i]], "\\{")[[1]][1]
				list_isos[i] <- paste(strsplit(cdata[thislist[i]+1], "")[[1]][40:42],collapse="")
				list_attestations[i] <- att(wordlist)
			}
			# taking care of the last list
			wordlist <- cdata[(nextlist[length(nextlist)] + 2):last_data_line]
			list_names[i] <- strsplit(cdata[thislist[i]], "\\{")[[1]][1]
			list_isos[i] <- paste(strsplit(cdata[thislist[i]+1], "")[[1]][40:42],collapse="")
			list_attestations[i] <- att(wordlist)
			# get rid of missing ISOs, which are ambiguous
			w_no_iso <- which(list_isos=="   ")
			if ( length(w_no_iso) > 0 ) {
				list_names <- list_names[-w_no_iso]
				list_isos <- list_isos[-w_no_iso]
				list_attestations <- list_attestations[-w_no_iso]
			}
			u_isos <- unique(list_isos)
			dfa <- data.frame(list_names, list_isos, list_attestations)
			dfa <- dfa[order(list_attestations, decreasing=TRUE),]
			m_u <- match(u_isos, dfa$list_isos)
			dfa_best_attested <- dfa[m_u,]
			dfa_incompletely_attested <- dfa_best_attested[which(dfa_best_attested$list_attestations < 28),]
			dfa_incompletely_attested <- dfa_incompletely_attested[order(dfa_incompletely_attested$list_attestations),]
			names(dfa_incompletely_attested) <- c("best att. doculect", "ISO-code", "attestations")
			if ( length(which(dfa_best_attested$list_attestations < 28)) > 0 ) {
				cat("There are ", length(which(dfa_best_attested$list_attestations < 28)), "languages whose best attested doculect has less than 28 items\n")
				cat("  --see", stats_file, "for a list.\n")
				cat("\nThere are ", length(which(dfa_best_attested$list_attestations < 28)), " languages whose best attested doculect has less than 28 items:\n", sep="", file=stats_file, append=TRUE)
				cat(names(dfa_incompletely_attested), sep="\t", file=stats_file, append=TRUE)
				cat("\n", file=stats_file, append=TRUE)
				write.table(dfa_incompletely_attested, file=stats_file, append=TRUE, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
			}
			# make list of languages in Glottolog not having ISO-codes
			# and missing; this will not produce false positives,
			# but a few may not be listed
			glot_nc <- glot[grep("NOCODE", glot$ISO639.3),]
			glot_nc <- glot_nc[-grep("Sign", glot_nc$ISO639.3),]
			# get indices for the doculects in ASJP 
			# that don't have ISO-codes
			w_asjp_nc <- union(which(is.na(iss)), grep("0", iss))
			# get the corresponding g classifications
			asjp_nc <- gcs[w_asjp_nc]
			missing_nocode_lgs <- c()
			count <- 0
			for (i in 1:nrow(glot_nc)) {
				if ( glot_nc$Classification[i] %in% asjp_nc == FALSE  ) {
					a1 <- glot_nc$ISO639.3[i]
					a2 <- strsplit(a1, "NOCODE_")[[1]][2]
					count <- count + 1
					missing_nocode_lgs[count] <- a2
				}
			}
			cat("\nGlottolog languages without ISO-codes not in ASJP (list possibly imprecise):\n")
			cat("\nGlottolog languages without ISO-codes not in ASJP (list possibly imprecise):\n", file=stats_file, append=TRUE)
			print(sort(missing_nocode_lgs))
			cat(sort(missing_nocode_lgs), sep=", ", file=stats_file, append=TRUE)
			# make a list of genera for which a 100-item list is not available
			gen_no_100 <- setdiff(wls_gen_all_unique, unique(wls_gen_all[which(lengths > 40)]))

			cat("\nGenera not yet represented by at least one 100-item list:\n")
			cat("\nGenera not yet represented by at least one 100-item list:\n", file=stats_file, append=TRUE)
			print(gen_no_100)
			cat(gen_no_100, sep=", ", file=stats_file, append=TRUE)
		}
		# choice was to output a tab-delimited file
		if ( length(grep("3", optEDITOR))==1 ) {
			cat("Please type the name of the tab-delimited output file and press ENTER ")
			optTABFILE <- readline()
			optTABFILE <- insist_overwrite_file(optTABFILE); regret(optTABFILE)
			parseASJP(optFILEEDIT, optTABFILE)
		}
		# choice was to update the Glottolog classification accessible to the program
		if ( length(grep("4", optEDITOR))==1 ) {
			cat("\n\nHave you made sure to download and unzip languoid.csv from\n     https://glottolog.org/meta/downloads\nand put it in the current folder?\n\n")
			cat("    1. Yes\n")
			cat("    2. No\n\n")
			cat("Type the number and press enter ")
			optGUPDATE <- readline()
			# choice was to update the Glottolog classification
			optGUPDATE <- fix_yn(optGUPDATE); optGUPDATE <- insist(optGUPDATE, "12x"); regret(optGUPDATE)
			if ( optGUPDATE=="1" ) {
				gfile()
				glot <<- read.table("glottolog.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
				cat("\n\nMetadata from Glottolog have been updated\n\n")
			}
		}
		# choice was to generate a file with metadata for missing languages
		if ( length(grep("5", optEDITOR))==1 ) {
			cat("\n\nMetadata will be generated for languages missing from\n")
			cat("the database currently in memory, and put in metadata.txt\n")

			## make necessary data accessible
			# WALS data is in wals-v2020.3.zip downloaded from https://zenodo.org/record/7385533
			# (doi: 10.5281/zenodo.7385533, version used: v2020.3)
			# get WALS data
			download.file("https://zenodo.org/record/7385533/files/cldf-datasets/wals-v2020.3.zip", "wals-v2020.3.zip")
			unzip("wals-v2020.3.zip", 
			  files=c("cldf-datasets-wals-878ea47/cldf/languages.csv"), 
			  junkpaths=TRUE)
			invisible(file.remove("wals-v2020.3.zip"))
			lgs <- read.csv(file="languages.csv")
			invisible(file.remove("languages.csv"))
			# family abbreviations are in fam_abbr, already in memory
			# ethnologue is in memory, as ethn
			# ASJP is in memory, as data_all
			# read the Glottolog file, if not in memory
			if ( exists("glot")==FALSE ) {
				glot <<- read.table("glottolog.tab", header=TRUE, sep="\t", quote="", stringsAsFactors=FALSE, na.strings="", comment.char="", encoding="UTF-8")
			}
			# for getting coordinates
			# use file downloaded from https://glottolog.org/meta/downloads
			glot_geo <- read.csv("languages_and_dialects_geo.csv")

			## prepare file
			cat("", file="metadata_missing.txt")

			## identify missing ISO-code languages and go through them
			# composing the metadata
			mi <- setdiff(ethn$ISO.639.3, data_all$iso)  # stands for missing isos
			for (i in 1:length(mi)) {
				# example:
				# XAJDAK_DARGI{NDa.DARGWIC|Nakh-Daghestanian,Dargi@Nakh-Daghestanian,Daghestanian,Dargwic}
				#  1   42.05   47.78           0   drg   xdq
				# X01{X02.X03|X04@X05}
				#  1     X06     X07         X08   X09   X10
				# set name of doculect
				X01 <- "NAME"
				# set WALS classification; is only done if there is a language with
				# an identical Glottolog classification
				w_g <- which(glot$ISO639.3 == mi[i])
				if ( length(w_g) > 0 ) {
					g_clas <- glot$Classification[w_g]
					w_g_clas <- which(data_all$hh==g_clas)[1]
					if ( !is.na(w_g_clas) ) {
						X02 <- data_all$wls_fam[w_g_clas]
						X03 <- data_all$wls_gen[w_g_clas]
						X02.X03 <- paste(X02, ".", X03, sep="")
					} else {
						X02.X03 <- "Fam.GENUS"
					}
				} else {
					X02.X03 <- "Fam.GENUS"
				}
				# set E classification
				w_e <- which(ethn$ISO.639.3 == mi[i])
				if ( length(w_e) > 0 ) {
					X04 <- gsub(" ", "", ethn$Classification[w_e])
				} else {
					X04 <- "EthnologueClassification"
				}
				# set Glottolog classification
				if ( length(w_g) > 0 ) {
					X05 <- glot$Classification[w_g]
				} else {
					X05 <- "GlottologClassification"
				}
				# set coordinates
				w_g_geo <- which(glot_geo$isocodes == mi[i])
				if ( length(w_g_geo) > 0 ) {
					lat <- as.character(glot_geo$latitude[w_g_geo])
					lon <- as.character(glot_geo$longitude[w_g_geo])
					if ( is.na(lat) | is.na(lon) ) {
						lat <- "        "
						lon <- "        "
					} else {
						# format latitude and longitude
						# case of no dot
						if ( length(grep("\\.", lat)) == 0 ) {
							lat <- paste(lat, ".00", sep="")
						}
						if ( length(grep("\\.", lon)) == 0 ) {
							lon <- paste(lon, ".00", sep="")
						}
						# case of a dot and one digit after it
						if ( length(grep("\\.", lat)) == 1 ) {
							if ( nchar(strsplit(lat, "\\.")[[1]][2])==1 ) {
								lat <- paste(lat, "0", sep="")
							}
						}
						if ( length(grep("\\.", lon)) == 1 ) {
							if ( nchar(strsplit(lon, "\\.")[[1]][2])==1 ) {
								lon <- paste(lon, "0", sep="")
							}
						}
						# case of more than two digits after the dot
						if ( length(grep("\\.", lat)) == 1 ) {
							if ( nchar(strsplit(lat, "\\.")[[1]][2]) > 2 ) {
								pre <- nchar(strsplit(lat, "\\.")[[1]][1])
								lat <- paste(strsplit(lat, "")[[1]][1:(pre+3)], collapse="")
							}
						}
						if ( length(grep("\\.", lon)) == 1 ) {
							if ( nchar(strsplit(lon, "\\.")[[1]][2]) > 2 ) {
								pre <- nchar(strsplit(lon, "\\.")[[1]][1])
								lon <- paste(strsplit(lon, "")[[1]][1:(pre+3)], collapse="")
							}
						}
						# add spaces before the coordinates
						length_lat <- nchar(as.character(lat))
						lat <- paste(paste(rep(" ", 8 - length_lat), collapse=""), lat, sep="")
						length_lon <- nchar(as.character(lon))
						lon <- paste(paste(rep(" ", 8 - length_lon), collapse=""), lon, sep="")
					}
				} else {
					lat <- "        "
					lon <- "        "					
				}
				X06 <- lat
				X07 <- lon
				# set population figure
				if ( length(w_e) > 0 ) {
					population <- ethn$Population.Numeric[w_e]
					if ( is.na(population) ) {
						X08 <- "0"
					} else if ( population == "0" ) {
						X08 <- "-X"
					} else {
						X08 <- population
					}
				} else {
					X08 <- "0"
				}
				length_pop <- nchar(as.character(X08))
				X08 <- paste(paste(rep(" ", 12 - length_pop), collapse=""), X08, sep="")
				# set WALS code
				w_lgs <- grep(mi[i], lgs$ISO_codes)
				if ( length(w_lgs) > 0 ) {
					if ( nchar(lgs$ID[w_lgs])==3 ) {
						X09 <- paste("   ", lgs$ID[w_lgs], sep="")
					} else {
						X09 <- "      "
					}
				} else {
					X09 <- "      "
				}
				# set ISO 639-3 code
				X10 <- paste("   ", mi[i], sep="")
				# output the metadata to a file
				# example:
				# X01{X02.X03|X04@X05}
				#  1     X06     X07         X08   X09   X10
				meta_out <- paste(X01, "{", X02.X03, "|", X04, "@", X05, "}\n 1", X06, X07, X08, X09, X10, "\n", sep="")
				cat(meta_out, file="metadata_missing.txt", append=TRUE)
			}
		}
	}
	cat("\n\nDo you want to continue the interactive session?\n\n")
	cat("    1. Yes\n")
	cat("    2. No\n\n")
	cat("Type the number and press enter ")
	optFIN <- readline()
	# choice was to continue the session
	optFIN <- fix_yn(optFIN); optFIN <- insist(optFIN, "12x"); regret(optFIN)
	if ( optFIN=="1" ) {
		if ( length(which(dir()=="greeting")) > 0 ) {
			invisible(file.remove("greeting"))
		}
		cat("\n")
		print(ASJP())
	}
	# choice was to end the session
	if ( optFIN=="2" ) {
		if ( "tmp_LDND_outfile.meg" %in% dir() ) {
			invisible(file.remove("tmp_LDND_outfile.meg"))
		}
		if ( "tmp_holman_in.txt" %in% dir() ) {
			invisible(file.remove("tmp_holman_in.txt"))
		}
		if ( optREC=="1" ) {
			credits <- readLines("credits.txt")
			cat(credits, file=logfile, sep="\n", append=TRUE)
		}
		exit_program(session_no, user)
	}
}
