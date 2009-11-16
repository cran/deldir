.First.lib <- function(lib,pkg) {
	library.dynam("deldir", pkg, lib)
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        cat(paste(pkg, ver, "\n"))
	cat(paste("\nPlease note: The process for determining duplicated points\n",
                  "has changed from that used in version 0.0-9.\n\n"))
}
