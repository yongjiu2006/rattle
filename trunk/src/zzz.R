.onLoad <- function(libname, pkgname)
{
  # 090207 Create the global crv environment for Rattle. Thus it is
  # always defined and plugins can override options. We generally
  # include here the options that can be overridden by a plugin.
  
  crv <<- new.env()

  crv$appname <<- "Rattle"
  crv$support.msg <<- "Contact support@togaware.com."
  crv$version <<- VERSION

  # Some global constants

  crv$close <<- "close"
  crv$verbose <<- TRUE # Add sub titles to plots ...
  crv$max.vars.correlation <<- 40 # Correlation slows down too much
  crv$export.to.c.available <<- FALSE # No export to C implemented yet
  crv$show.warnings <<- TRUE # 090207 Show test/train warning.
  crv$project.extensions <<- c("*.rattle", "*.rstat") # Extensions for projects  

}

# 080417 The R manual for .onLoad says to use .onAttach for startup
# messages.

.onAttach <- function(libname, pkgname)
{
  # This is executed when the package becomes visible to the user.
  
  # 090206 How to not display the welcom message if quietly=TRUE?
  # Otherwise it is annoying, just like fBasics. randomForest seems to
  # do it correctly? Maybe not.... One solution is for the user to
  # define suppressRattleWelcome and set it to TRUE before loading
  # Rattle.

  if (! (exists("suppressRattleWelcome") && suppressRattleWelcome))
  {

    cat(sprintf(paste("Rattle, Graphical interface for data mining using R\n",
                      "Version %s. ", COPYRIGHT, "\n", sep=""), VERSION))
    if ("rattle" %in% getOption("defaultPackages"))
      rattle()
    else
      cat(paste("Type \"rattle()\" to shake, rattle, and roll ",
                "your data.\n", sep=""))
  }
}

