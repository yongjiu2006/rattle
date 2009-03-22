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
  
  crv$appname 		<- "Rattle"
  crv$log.intro 	<- "# Rattle is Copyright (C) 2006-2009 Togaware Pty Ltd"
  crv$support.msg 	<- "Contact support@togaware.com."
  crv$library.command 	<- "library(rattle)"
  crv$version 		<- VERSION

  # Some global constants

  crv$show.timestamp		<- FALSE
  crv$tooltiphack 		<- FALSE
  crv$close 			<- "close"
  crv$sample.dataset 		<- "audit"
  if (.Platform$OS.type == "unix")
    crv$load.tooltips 		<- FALSE # Not working in general on Linux
  else
    crv$load.tooltips 		<- TRUE
  crv$verbose 			<- TRUE # Add sub titles to plots ...
  crv$max.vars.correlation 	<- 40 # Correlation slows down too much
  crv$export.to.c.available 	<- FALSE # No export to C implemented yet
  crv$show.warnings 		<- TRUE # 090207 Show test/train warning.
  crv$project.extensions 	<- c("rattle", "rstat") # Extensions for projects  
  crv$ident.min.rows 		<- 300 # Unique factors/ints > than this are idents

  # Log constants

  crv$start.log.comment <- "\n\n# "	# Assume paste with sep=""
  crv$end.log.comment   <- "\n\n"	# Assume paste with sep=""
  
  # Model defaults

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
  
  if (! suppressRattleWelcome)
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

