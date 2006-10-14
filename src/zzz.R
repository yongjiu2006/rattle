.onLoad <- function(libname, pkgname)
{
  ## TODO: How to not do this if quietly=TRUE? Otherwise it will be
  ## annoying, just like fBasics. randomForest seems to do it
  ## correctly? Maybe not....
  
  cat(sprintf(paste("Rattle, Graphical interface for data mining using R,",
                    "Version %s.",
                    "\nCopyright (c) 2006, Graham Williams,",
                    "rattle.togaware.com, GPL",
                    "\nType \"rattle()\" to shake, rattle, and roll",
                    "your data.\n"),
              VERSION))
}

