.onLoad <- function(lib, pkg) {
	library.dynam("deldir", pkg, lib)
}

.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        nick <- "Nickname: \"Stack Smashing Detected\""
        # 02/11/2020
        # Next one (???):
        #nick <- "Nickname: \"Morpheus and Euripides\""
        #nick <- "Nickname: \"Dyslexical Scoping\""
        #nick <- "Nickname: \"Mendacious Cosmonaut\""
        #nick <- "Nickname: \"Partial Distinction\""
        #nick <- "Nickname: \"Mephistophelian Transition\""
        #nick <- "Nickname: \"Idol Comparison\""
        #nick <- "Nickname: \"Perspicuous Ambivalence\""
        packageStartupMessage(paste(pkg, ver, "    ",nick))
        msg <- paste("\n     Note 1: As of version 0.2-1, error handling in this",
                     "\n     package was amended to conform to the usual R protocol.",
                     "\n     The deldir() function now actually throws an error",
                     "\n     when one occurs, rather than displaying an error number",
                     "\n     and returning a NULL.\n",
                     "\n     Note 2:  As of version 0.1-29 the arguments \"col\"",
                     "\n     and \"lty\" of plot.deldir() had their names changed to",
                     "\n     \"cmpnt_col\" and \"cmpnt_lty\" respectively basically",
                     "\n     to allow \"col\" and and \"lty\" to be passed as \"...\"",
                     "\n     arguments.\n",
                     "\n     Note 3: As of version 0.1-29 the \"plotit\" argument",
                     "\n     of deldir() was changed to (simply) \"plot\".\n",
                     "\n     See the help for deldir() and plot.deldir().\n")
        packageStartupMessage(msg)
}
