.First.lib <- function(lib,pkg) {
	library.dynam("deldir", pkg, lib)
	cat("deldir\nversion: 0.0-2\n")
}
