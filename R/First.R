.onLoad <- function(lib, pkg) {
	library.dynam("deldir", pkg, lib)
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        packageStartupMessage(paste(pkg, ver))
	packageStartupMessage(
            paste( "\n     PLEASE NOTE:  The components \"delsgs\" and \"summary\"",
                  "of the", "\n     object returned by deldir() are now",
                  "DATA FRAMES rather than","\n     matrices (as they were prior",
                  "to release 0.0-18).",    "\n     See help(\"deldir\").\n",
                  "\n     PLEASE NOTE: The process for determining duplicated",
                  "points\n     has changed from that used in version 0.0-9",
                  "(and previously).\n"))
}
