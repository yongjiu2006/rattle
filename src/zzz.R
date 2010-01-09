.onLoad <- function(libname, pkgname)
{
  # 090315 Create the crs environment here. It is defined here (and
  # then also reset in rattle() so that R CMD check would not complain
  # about knowing nothing of crs (after removing the crs<<- assigments
  # throughout rattle)!

  crs <<- new.env()

  # 090207 Create the global crv environment for Rattle. Once again,
  # this is a deviation from Chamber's Prime Directive, but is akin to
  # the use of option.  It is defined here so that it is glabally
  # known and so that plugins can override options. We generally
  # include here the options that can be overridden by a plugin.
  
  crv <<- new.env()
  
  crv$appname <- "Rattle"
  crv$projext <- ".rattle"
  crv$log.intro <- "# Rattle is Copyright (C) 2006-2010 Togaware Pty Ltd"
  crv$support.msg <- "Contact support@togaware.com."
  crv$library.command <- "library(rattle)"
  crv$version <- VERSION

  # Some global constants

  # 091130 Use UTF-8 as the default encoding for files. This certainly
  # works okay on GNU/Linux. On Vista I see ISO8859-1 as the default
  # and Acken sees CP932 for Japanese.
  
  crv$csv.encoding <- "UTF-8"
  
  crv$show.timestamp <- FALSE
  ## crv$tooltiphack <- FALSE
  crv$close <- "ask"
  # crv$sample.dataset <- "audit"
  crv$sample.dataset <- "weather"
  
  # 090525 Always load tooltips - now use Settings option to enable on
  # GNU/Linux. 090622 But on older installations we still get the
  # Invalid property error so for now on Unix do not support tooltips.
  
  # 090601 Add the crv$load.tooltips option, so it can be turned off
  # on the command line before starting rattle, since older GTK
  # version has issue: Invalid property tooltip-text!
  
  crv$load.tooltips <- TRUE
  
  if (.Platform$OS.type == "unix")
    crv$load.tooltips <- FALSE # Not working in general on Linux
  
  crv$verbose <- TRUE # Add sub titles to plots ...

  crv$max.categories <- 10 # Above which target assumed numeric, not categoric
  crv$max.vars.correlation <- 40 # Correlation slows down too much
  crv$export.to.c.available <- FALSE # No export to C implemented yet
  crv$show.warnings <- TRUE # 090207 Show test/train warning.
  crv$project.extensions <- c("rattle", "rstat") # Extensions for projects  
  crv$ident.min.rows <- 300 # Unique factors/ints > than this are idents
  crv$default.train.percentage <- 70 # The default sample percentage value.
  
  # Log constants

  crv$start.log.comment <- "\n\n# "	# Assume paste with sep=""
  crv$end.log.comment   <- "\n\n"	# Assume paste with sep=""
  
  # Model defaults

  crv$cluster.report.max.obs <- 4000
  crv$scatter.max.vars <- 5
  
  crv$rpart.cp.default        <- 0.010
  crv$rpart.minsplit.default  <- 20
  crv$rpart.minbucket.default <- 7
  crv$rpart.maxdepth.default  <- 30

  crv$ada.ntree.default   <- 50

  crv$rf.ntree.default    <- 500
  crv$rf.mtry.default     <- 10
  crv$rf.sampsize.default <- ""

  # Evaluate

  .RATTLE.DATA <<- NULL
  .RATTLE.SCORE.IN <<- NULL
  .RATTLE.SCORE.OUT <<- NULL
  
  # 090309 We set some other global variables for convenience.

  rattleGUI <<- NULL
  Global_rattleGUI <<- NULL
  viewdataGUI <<- NULL
  on_aboutdialog_response <<- NULL

}

# 080417 The R manual for .onLoad says to use .onAttach for startup
# messages.

.onAttach <- function(libname, pkgname)
{
  # This is executed when the package becomes visible to the user.
  
  # 090206 How to not display the welcome message if quietly=TRUE?
  # Otherwise it is annoying, just like fBasics. randomForest seems to
  # do it correctly? Maybe not.... One solution is for the user to
  # define suppressRattleWelcome and set it to TRUE before loading
  # Rattle. Note that we can't do this with
  # crv$suppress.rattle.wanring, defined in zzz.R of rattle, inside
  # .onLoad, since they get loaded and attached before rstat does.

  if (! exists("suppressRattleWelcome")) suppressRattleWelcome <<- FALSE

  # 091221 The Rtxt does not seem to work from the rattle.R file, so
  # do it here again.
  
  COPYRIGHT <- paste(Rtxt("Copyright"), "(C) 2006-2010 Togaware Pty Ltd.")

  if (! suppressRattleWelcome)
  {

    cat(sprintf(paste(Rtxt("Rattle: Graphical interface for data mining using R."),
                      Rtxt("\nVersion"),
                      " %s ", COPYRIGHT, "\n", sep=""), VERSION))
    if ("rattle" %in% getOption("defaultPackages"))
      rattle()
    else
      cat(Rtxt("Type 'rattle()' to shake, rattle, and roll your data.\n"))
  }
}
