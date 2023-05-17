.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        #nick <- "Nickname: \"Stack Smashing Detected\""
        #nick <- "Nickname: \"Morpheus and Euripides\""
        #nick <- "Nickname: \"Dyslexical Scoping\""
        #nick <- "Nickname: \"Mendacious Cosmonaut\""
         nick <- "Nickname: \"Partial Distinction\""
        #nick <- "Nickname: \"Mephistophelian Transition\""
        #nick <- "Nickname: \"Idol Comparison\""
        #nick <- "Nickname: \"Perspicuous Ambivalence\""
        #nick <- "Nickname: \"Cats are not required to wear seatbelts\""
        #nick <- "Nickname: \"There weren't many available options\""
        packageStartupMessage(paste(pkg, ver, "    ",nick))
        msg <- paste("\n     The syntax of deldir() has changed since version",
                     "\n     0.0-10.  In particular the \"dummy points\" facility",
                     "\n     (which was a historical artifact) has been removed.",
                     "\n     In the current version, 1.0-8, an argument \"id\" has",
                     "\n     been added to deldir().  This new argument permits the",
                     "\n     user to specifier identifiers for points.  The default",
                     "\n     behaviour is to continue using the indices of the",
                     "\n     points to identify them.  In view of the fact that",
                     "\n     point identifiers may be user-supplied, the arguement",
                     "\n     \"number\", in plot.deldir() and plot.tile.list(), has",
                     "\n     had its name changed to \"labelPts\", and the argument",
                     "\n     \"nex\" in plot.deldir() has had its name changed to",
                     "\n     \"lex\".  In addition the name of the forth component",
                     "\n     of the \"cmpnt_col\" argument in plot.deldir() has been",
                     "\n     changed from \"num\" to \"labels\".  There is a new",
                     "\n     function getNbrs(), and the function tileInfo() has",
                     "\n     been modified to include output from getNbrs().",
                     "\n     Please consult the help.\n")
        packageStartupMessage(msg)
}
