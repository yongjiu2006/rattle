#.onLoad <- function(libname, pkgname)
#{
#}

# 080417 The R manual for .onLoad says to use .onAttach for startup
# messages.

.onAttach <- function(libname, pkgname)
{
  # This is executed when the package becomes visible to the user.
  
  # TODO: How to not do this if quietly=TRUE? Otherwise it will be
  # annoying, just like fBasics. randomForest seems to do it
  # correctly? Maybe not....
  
  cat(sprintf(paste("Rattle, Graphical interface for data mining using R\n",
                    "Version %s. ", COPYRIGHT, "\n", sep=""), VERSION))
  if ("rattle" %in% getOption("defaultPackages"))
    rattle()
  else
    cat(paste("Type \"rattle()\" to shake, rattle, and roll ",
              "your data.\n", sep=""))
}

