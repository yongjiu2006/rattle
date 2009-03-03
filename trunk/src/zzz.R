.onLoad <- function(libname, pkgname)
{
  # 090207 Create the global crv environment for Rattle. Thus it is
  # always defined and plugins can override options. We generally
  # include here the options that can be overridden by a plugin.
  
  crv <<- new.env()

  crv$appname <<- "Rattle"
  crv$log.intro <<- "# Rattle is Copyright (C) 2006-2009 Togaware Pty Ltd"
  crv$support.msg <<- "Contact support@togaware.com."
  crv$library.command <<- "library(rattle)"
  crv$version <<- VERSION

  # Some global constants

  crv$close <<- "close"
  crv$sample.dataset <<- "audit"
  if (.Platform$OS.type == "unix")
    crv$load.tooltips <<- FALSE
  else
    crv$load.tooltips <<- TRUE
  crv$verbose <<- TRUE # Add sub titles to plots ...
  crv$max.vars.correlation <<- 40 # Correlation slows down too much
  crv$export.to.c.available <<- FALSE # No export to C implemented yet
  crv$show.warnings <<- TRUE # 090207 Show test/train warning.
  crv$project.extensions <<- c("rattle", "rstat") # Extensions for projects  
  crv$ident.min.rows <<- 300 # Unique factors/ints larger than this are idents.

  # Model defaults

  crv$rf.ntree.default    <<- 500
  crv$rf.mtry.default     <<- 10
  crv$rf.sampsize.default <<- ""

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

