.onLoad <- function(lib, pkg) {
	library.dynam("deldir", pkg, lib)
}

.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        packageStartupMessage(paste(pkg, ver))
        msg <- paste("\n     Please note that the arguments \"col\" and",
                     "\n     \"lty\" of plot.deldir() have had their",
                     "\n     names changed to \"cmpnt_col\" and \"cmpnt_lty\"",
                     "\n     respectively, basically to allow \"col\" and",
                     "\n     and \"lty\" to be passed as \"...\" arguments.",
                     "\n     Also the \"plotit\" argument of deldir() has",
                     "\n     been changed to (simply) \"plot\".",
                     "\n     See the help for plot.deldir() and deldir().\n")
        packageStartupMessage(msg)
}
