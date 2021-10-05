.onAttach <- function(lib, pkg) {
	ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
        #nick <- "Nickname: \"Stack Smashing Detected\""
        # 02/11/2020
        # nick <- "Nickname: \"Morpheus and Euripides\""
        # 12/05/2021
        #nick <- "Nickname: \"Dyslexical Scoping\""
        nick <- "Nickname: \"Mendacious Cosmonaut\""
        #nick <- "Nickname: \"Partial Distinction\""
        #nick <- "Nickname: \"Mephistophelian Transition\""
        #nick <- "Nickname: \"Idol Comparison\""
        #nick <- "Nickname: \"Perspicuous Ambivalence\""
        packageStartupMessage(paste(pkg, ver, "    ",nick))
        msg <- paste("\n     The syntax of deldir() has had an important change.",
                     "\n     The arguments have been re-ordered (the first three",
                     "\n     are now \"x, y, z\") and some arguments have been",
                     "\n     eliminated.  The handling of the z (\"tags\")",
                     "\n     argument has been improved.\n",
                     "\n     The \"dummy points\" facility has been removed.",
                     "\n     This facility was a historical artefact, was really",
                     "\n     of no use to anyone, and had hung around much too",
                     "\n     long.  Since there are no longer any \"dummy points\",",
                     "\n     the structure of the value returned by deldir() has",
                     "\n     changed slightly.  The arguments of plot.deldir()",
                     "\n     have been adjusted accordingly; e.g. the character",
                     "\n     string \"wpoints\" (\"which points\") has been",
                     "\n     replaced by the logical scalar \"showpoints\".",
                     "\n     The user should consult the help files.\n")
        packageStartupMessage(msg)
}
