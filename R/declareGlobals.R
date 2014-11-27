# Utility file to add ".Last.make.date" to the list of global
# variables, so as to avoid the "no visible binding" problem in
# make.fun() that shows up if .Last.make.date does not exist.

if(getRversion() >= "2.15.1")
    utils::globalVariables("polyclip")
