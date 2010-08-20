# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2010-08-15 21:26:01 Graham Williams>
#
# CTREE OPTION OF THE TREE TAB
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# This files is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

########################################################################
#
# Model -> Tree -> Conditional
#

# 100815 TODO The "partykit" package from R-Forge (only for now)
# includes .list.rules.party() to convert tree into rules:
#
# install.packages("partykit", repos = "http://R-Forge.R-project.org")
# library("partykit")
# Rebuild the ctree as partykit provides new ctree.
# partykit:::.list.rules.party(crs$rpart)

executeModelCTree <- function()
{
  # 080815 This is currently just copied from rpart.R, and slowly
  # being tuned for ctree specifically.
  
  # Initial setup 

  TV <- "rpart_textview"

  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
  control <- NULL
  parms <- NULL

  # Scrape the value of the tuning controls

  tune.controls <- theWidget("rpart_tune_entry")$getText()
  
  # Retrieve the Priors, and check there is the right number and that
  # they add up to 1.
  
  priors <- theWidget("model_tree_priors_entry")$getText()
  if (nchar(priors) > 0)
  {
    pr <- as.numeric(unlist(strsplit(priors, ",")))
    if (length(pr) != num.classes)
      {
        errorDialog(sprintf(Rtxt("The supplied priors (%s)",
                                 "need to correspond to the number of classes",
                                 "found in the target variable '%s'.",
                                 "Please supply exactly %d priors."),
                            priors, crs$target, num.classes))
        return(FALSE)
      }
    if (sum(pr) != 1)
      {
        errorDialog(sprintf(Rtxt("The supplied priors (%s)",
                                 "add up to %0.2f whereas",
                                 "they need to add up 1.00.",
                                 "Please provide appropriate priors."),
                            priors, sum(pr)))
        return(FALSE)
      }
    if (is.null(parms))
      parms <- sprintf(", parms=list(prior=c(%s))", priors)
    else
      parms <- gsub(")$", sprintf(", prior=c(%s)", priors), parms)
  }

  # Retrieve the Min Split and check if it is different from the
  # default, and if so then use it.

  minsplit <- theWidget("rpart_minsplit_spinbutton")$getValue()
  if (minsplit != crv$rpart.minsplit.default)
  {
    if (is.null(control))
      control <- sprintf(", control=ctree_control(minsplit=%d)", minsplit)
    else
      control <- gsub(")$", sprintf(", minsplit=%d)", minsplit), control)
  }

  # Retrieve the Min Bucket and check if it is different from the
  # default, and if so then use it.

  minbucket <- theWidget("rpart_minbucket_spinbutton")$getValue()
  if (minbucket != crv$rpart.minbucket.default)
  {
    if (is.null(control))
      control <- sprintf(", control=ctree_control(minbucket=%d)", minbucket)
    else
      control <- gsub(")$", sprintf(", minbucket=%d)", minbucket), control)
  }

  # Retrieve the Max Depth and check if it is different from the
  # default, and if so then use it.

  maxdepth <- theWidget("rpart_maxdepth_spinbutton")$getValue()
  if (maxdepth != crv$rpart.maxdepth.default)
  {
    if (is.null(control))
      control <- sprintf(", control=ctree_control(maxdepth=%d)", maxdepth)
    else
      control <- gsub(")$", sprintf(", maxdepth=%d)", maxdepth), control)
  }

  # Retrieve the Complexity and check if it is different from the
  # default, and if so then use it.

##   cp <- theWidget("model_tree_cp_spinbutton")$getValue()

##   if (abs(cp-crv$rpart.cp.default) > 0.00001) ## Diff when same is 2.2352e-10!!!
##   {
##     if (is.null(control))
##       control <- sprintf(", control=rpart.control(cp=%f)", cp)
##     else
##       control <- gsub(")$", sprintf(", cp=%f)", cp), control)
##   }

  # Retrieve the Include Missing checkbutton status and if not set
  # then change default beahviour in usesurrogate.

##   usesurrogate <- theWidget("model_tree_include_missing_checkbutton")$getActive()
##   if (! usesurrogate)
##   {
##     if (is.null(control))
##       control <- ", control=rpart.control(usesurrogate=0)"
##     else
##       control <- gsub(")$", ", usesurrogate=0)", control)
##   }

  # Retrieve the Cross Validation value and if different from
  # default, use it. No longer. Common wisdom is that 10 is right, so
  # in Rattle just go with that.
  
  # xval <- theWidget("rpart_xval_spinbutton")$getValue()
  # if (xval != crv$rpart.xval.default)
  # {
  #  if (is.null(control))
  #    control <- sprintf(", control=rpart.control(xval=%d)", xval)
  #  else
  #    control <- gsub(")$", sprintf(", xval=%d)", xval), control)
  # }

  # Retrieve the loss matrix and ensure it matches the shape of the
  # data.

##   loss <- theWidget("model_tree_loss_entry")$getText()
##   if (nchar(loss) > 0)
##   {
##     lo <- as.numeric(unlist(strsplit(loss, ",")))
##     if (length(lo) != num.classes * num.classes)
##     {
##       errorDialog(sprintf("The supplied loss matrix values (%s)", loss),
##                    sprintf("need to have %d values.", num.classes*num.classes),
##                    "Please enter that many values, comma separated.")
##       return(FALSE)
##     }
      
##     # TODO: Perform other checks on the matrix here.  The loss matrix
##     # must have zeros on the diagonal and positive numbers
##     # elsewhere. It must be the same dimensions as the number of
##     # classes.

##     lo <- sprintf("matrix(c(%s), byrow=TRUE, nrow=%d)", loss, num.classes) 
    
##     if (is.null(parms))
##       parms <- sprintf(", parms=list(loss=%s)", lo)
##     else
##       parms <- gsub(")$", sprintf(", loss=%s)", lo), parms)
##   }

  # Build the formula for the model. Rpart has only a formula
  # interface.

  frml <- paste(crs$target, "~ .")

  # Variables to be included --- a string of indicies.
  
  included <- getIncludedVariables()
  
  # Some convenience booleans

  sampling  <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling || including
  
  # Commands.
  
  lib.cmd <- "require(party, quietly=TRUE)"
  if (! packageIsAvailable("party", Rtxt("build conditional trees"))) return(FALSE)

##   if (action %in%  c("tune", "best"))
##   {
##     lib.cmd <- paste(lib.cmd, "require(e1071, quietly=TRUE)", sep="\n")
##     if (! packageIsAvailable("e1071", "tune decision trees")) return(FALSE)
##   }

##   # For now, don't use any of the other parameter settings if tune or
##   # best. Eventually I want to use the other parameter settings and
##   # override them with the tune options.

##   if (action == "build")
##   {
    fit.cmd <- paste("crs$rpart <- ctree(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
#                       ifelse(is.null(crs$weights), "",
#                              sprintf(", weights=(%s)%s",
#                                      crs$weights,
#                                      ifelse(sampling, "[crs$sample]", ""))),
#                       ', method=',
#                       ifelse(categoricTarget(),
#                              '"class"', '"anova"'),
#                       ifelse(is.null(parms), "", parms),
                       ifelse(is.null(control), "", control),
                     ")", sep="")

  print.cmd <- "print(crs$rpart)"
##   }
##   else if (action == "tune")
##   {
##     rpart.cmd <- paste("crs$tune.rpart <- tune.rpart(", frml, ", data=crs$dataset",
##                      if (subsetting) "[",
##                      if (sampling) "crs$sample",
##                      if (subsetting) ",",
##                      if (including) included,
##                      if (subsetting) "]",
##                      sprintf(", %s", tune.controls),
##                      ")", sep="")

##     print.cmd <- paste("print(crs$tune.rpart)", "plot(crs$tune.rpart)", sep="\n")
##   }
##   else if (action == "best")
##   {
##     # This won't work - best.rpart usese the tune.control() structure
##     rpart.cmd <- paste("crs$rpart <- best.rpart(", frml, ", data=crs$dataset",
##                      if (subsetting) "[",
##                      if (sampling) "crs$sample",
##                      if (subsetting) ",",
##                      if (including) included,
##                      if (subsetting) "]",
##                      sprintf(", %s", tune.controls),
##                      ")", sep="")

##     print.cmd <- paste("print(crs$rpart)", "printcp(crs$rpart)", sep="\n")
##   }
                               
  # Load the required library.

  startLog(Rtxt("Conditional inference tree."))
  appendLog(Rtxt("Build a conditional tree using the party package."), lib.cmd)

  eval(parse(text=lib.cmd))

  # Set the seed so that xerror and xstd are consistent each time

#  seed.cmd <- 'set.seed(crv$seed)'
#  appendLog("Set the seed to ensure same cross validation results each time.", seed.cmd)
#  eval(parse(text=seed.cmd))

  # Build the model.

  appendLog(Rtxt("Build a ctree model."), fit.cmd)
  start.time <- Sys.time()
  result <- try(eval(parse(text=fit.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time
  if (inherits(result, "try-error"))
  {
    errorDialog(errorMessageFun("ctree", result))
    return(FALSE)
  }

  # Display the resulting model.

  appendLog(Rtxt("Generate summary of the ctree model."), print.cmd)

  resetTextview(TV)
  setTextview(TV,
              sprintf(Rtxt("Summary of the %s model for %s (built using '%s'):\n"),
                      commonName("ctree"),
                      Rtxt("Classification"), # 080604 TODO put the right type
                      "ctree"),
              collectOutput(print.cmd), "\n")

  if (sampling) crs$smodel <- union(crs$smodel, crv$RPART)

  # Now that we have a model, make sure the rules and plot buttons are
  # visible.
  
  showModelRPartExists()

  # Finish up.

  reportTimeTaken(TV, time.taken, model=commonName(crv$RPART))

  return(TRUE)
}

## showModelRPartExists <- function(state=!is.null(crs$rpart))
## {
##   # If an rpart model exists then show the Rules and Draw buttons on
##   # the Model tab.
  
##   if (state)
##   {
##     theWidget("rpart_plot_button")$show()
##     theWidget("rpart_plot_button")$setSensitive(TRUE)
##     theWidget("rpart_rules_button")$show()
##     theWidget("rpart_rules_button")$setSensitive(TRUE)
##   }
##   else
##   {
##     theWidget("rpart_plot_button")$hide()
##     theWidget("rpart_rules_button")$hide()
##   }    
## }

## #------------------------------------------------------------------------
## # Modified version or print.rpart
## #

## rattle.print.rpart <- function (x, minlength = 0, spaces = 2, cp,
##                                 digits = getOption("digits"), ...) 
## {
##     if (!inherits(x, "rpart")) 
##         stop("Not legitimate rpart object")
##     if (!is.null(x$frame$splits)) 
##         x <- rpconvert(x)
##     if (!missing(cp)) 
##         x <- prune.rpart(x, cp = cp)
##     frame <- x$frame
##     ylevel <- attr(x, "ylevels")
##     node <- as.numeric(row.names(frame))
##     depth <- rpart:::tree.depth(node)
##     indent <- paste(rep(" ", spaces * 32), collapse = "")
##     if (length(node) > 1) {
##         indent <- substring(indent, 1, spaces * seq(depth))
##         indent <- paste(c("", indent[depth]), format(node), ")", 
##             sep = "")
##     }
##     else indent <- paste(format(node), ")", sep = "")
##     tfun <- (x$functions)$print
##     if (!is.null(tfun)) {
##         if (is.null(frame$yval2)) 
##             yval <- tfun(frame$yval, ylevel, digits)
##         else yval <- tfun(frame$yval2, ylevel, digits)
##     }
##     else yval <- format(signif(frame$yval, digits = digits))
##     term <- rep(" ", length(depth))
##     term[frame$var == "<leaf>"] <- "*"
##     z <- labels(x, digits = digits, minlength = minlength, ...)
##     n <- frame$n
##     # DEV is residual sum of squares
##     z <- paste(indent, z, n, format(signif(sqrt(frame$dev), digits = digits)),
##         yval, term)
##     omit <- x$na.action
##     if (length(omit)) 
##         cat("n=", n[1], " (", naprint(omit), ")\n\n", sep = "")
##     else cat("n=", n[1], "\n\n")
##     if (x$method == "class") 
##         cat("node), split, n, loss, yval, (yprob)\n")
##     else cat("node), split, n, SQRT(deviance), yval\n")
##     cat("      * denotes terminal node\n\n")
##     cat(z, sep = "\n")
##     return(invisible(x))
## }

## #----------------------------------------------------------------------
## # Print out RPart Rules

## listRPartRules <- function(model, compact=FALSE)
## {
##   if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
##   # if (model$method != "class")) stop("Model method needs to be class")
##   #
##   # Get some information.
##   #
##   rtree <- length(attr(model, "ylevels")) == 0
##   frm <- model$frame
##   names <- row.names(frm)
##   ylevels <- attr(model, "ylevels")
##   ds.size <-  model$frame[1,]$n
##   #
##   # Print each leaf node as a rule.
##   #
##   if (rtree)
##     # Sort rules by coverage
##     ordered <- rev(sort(frm$n, index=TRUE)$ix)
##   else
##     # Sort rules by probabilty of second class (usually the last in binary class)
##     ordered <- rev(sort(frm$yval2[,5], index=TRUE)$ix)
##   for (i in ordered)
##   {
##     if (frm[i,1] == "<leaf>")
##     {
##       # The following [,5] is hardwired and works on one example....
##       if (rtree)
##         yval <- frm[i,]$yval
##       else
##         yval <- ylevels[frm[i,]$yval]
##       cover <- frm[i,]$n
##       pcover <- round(100*cover/ds.size)
##       if (! rtree) prob <- frm[i,]$yval2[,5]
##       cat("\n")
##       pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
##       pth <- unlist(pth)[-1]
##       if (length(pth) == 0) pth <- "True"
##       if (compact)
##       {
##         cat(sprintf("R%03s ", names[i]))
##         if (rtree)
##           cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
##         else
##           cat(sprintf("[%2.0f%%,%0.2f]", pcover, prob))
##         cat(sprintf(" %s", pth), sep="")
##       }
##       else
##       {
##         cat(sprintf(" Rule number: %s ", names[i]))
##         if (rtree)
##           cat(sprintf("[yval=%s cover=%d (%.0f%%)]\n",
##                       yval, cover, pcover))
##         else
##           cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
##                       yval, cover, pcover, prob))
##         cat(sprintf("   %s\n", pth), sep="")
##       }
##     }
##   }
##   cat("\n")
##   invisible(ordered)
## }

## list.rule.nodes.rpart <- function(model)
## {
##   # The information we need is in the rpart frame
##   frm <- model$frame
##   # Obtain the probabilities
##   nodes <- frm$yval2[,5]
##   # Get the probability ordered index of leaf nodes
##   ordered <- sort(frm$yval2[,5][frm[,1] == "<leaf>"],
##                   decr=TRUE, index=TRUE)
##   # Return the list of node numbers
##   return(row.names(frm)[which(frm[,1] == "<leaf>")][ordered$ix])
## }

## #
## # Modify draw.tree from maptree to draw rule nodes rather than
## # sequential numbers to label the leaves.
## #
## # 080315 change size from 2.5 to 4 so the percentages are not cramped
## # up---they did not use to but with a new version of R (not sure which
## # one) they did.
## #
## # 080315 Rob Williams noted that when using loss (e.g., 0,4,1,0) the
## # percentages being displayed are selecting the larger probabiliy, not
## # the correct probability.
## #
## drawTreeNodes <- function (tree, cex = par("cex"), pch = par("pch"),
##                            size = 4 * cex, col = NULL, nodeinfo = FALSE,
##                            units = "", cases = "cases", 
##                            digits = getOption("digits"),
##                            decimals = 2,
##                            print.levels = TRUE, new = TRUE) 
## {
##   if (new) plot.new()

##   rtree <- length(attr(tree, "ylevels")) == 0
##   tframe <- tree$frame
##   rptree <- length(tframe$complexity) > 0
##   node <- as.numeric(row.names(tframe))
##   leafnode <- node[tframe$var == "<leaf>"]
##   #proportions <- sprintf("%0.2f", tframe$yval2[,5])
##   depth <- floor(log(node, base = 2) + 1e-07)
##   depth <- as.vector(depth - min(depth))
##   maxdepth <- max(depth)
##   x <- -depth
##   y <- x
##   leaves <- tframe$var == "<leaf>"
##   x[leaves] <- seq(sum(leaves))
##   depth <- split(seq(node)[!leaves], depth[!leaves])
##   parent <- match(node%/%2, node)
##   left.child <- match(node * 2, node)
##   right.child <- match(node * 2 + 1, node)
##   for (i in rev(depth)) x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
##   nleaves <- sum(leaves)
##   nnodes <- length(node)
##   if (rtree)
##   {
##     dev <- tframe$dev
##     pcor <- rep(0, nnodes)
##     for (i in 1:nnodes) if (!leaves[i]) {
##       l <- dev[node == (node[i] * 2)]
##       r <- dev[node == (node[i] * 2 + 1)]
##       pcor[i] <- dev[i] - l - r
##     }
##     pcor <- round(pcor/dev[1], 3) * 100
##   }
##   else
##   {
##     crate <- rep(0, nnodes)
##     trate <- 0
##     if (!rptree)
##     {
##       for (i in 1:nnodes)
##       {
##         yval <- tframe$yval[i]
##         string <- paste("tframe$yprob[,\"", as.character(yval), 
##                         "\"]", sep = "")
##         crate[i] <- eval(parse(text = string))[i]
##         if (leaves[i]) 
##           trate <- trate + tframe$n[i] * crate[i]
##       }
##     }
##     else
##     {
##       for (i in 1:nnodes)
##       {
##         yval <- tframe$yval[i]
##         nlv <- floor(ncol(tframe$yval2)/2)
##         # [080315 gjw] Now sort the class rates and get the largest!!!
##         # But wouldn't we want to get the one corresponding to yval
##         # rather than the largest? It won't necessarily be the largest
##         # the when we use a loss matrix? The original code from
##         # draw.tree uses the largest, but for my purposes I was
##         # wanting the correct one! So I keep the index calculation
##         # (for notation) but replace it in the crate assignment with
##         # yval instead. With this change I don't think I affect too
##         # much. The trate variable is affected, but it is only printed
##         # with nodeinfo set to TRUE. I'm not sure the original is
##         # correct though. This is reported as the "total classified
##         # correct."
##         index <- rev(order(tframe$yval2[i, 2:(nlv + 1)]))[1]
##         # ORIG crate[i] <- tframe$yval2[i, (nlv + 1 + index1)]
##         crate[i] <- tframe$yval2[i, (nlv + 1 + yval)]
##         if (leaves[i])
##         {
##           trate <- trate + tframe$n[i] * crate[i]
##         }
##       }
##     }
##     crate <- round(crate, 3) * 100
##     trate <- round(trate/tframe$n[1], 3) * 100
##   }
##   if (is.null(col)) 
##     kol <- rainbow(nleaves)
##   else if (col == "gray" | col == "grey") 
##     kol <- gray(seq(0.8, 0.2, length = nleaves))
##   else kol <- col
##   xmax <- max(x)
##   xmin <- min(x)
##   ymax <- max(y)
##   ymin <- min(y)
##   pinx <- par("pin")[1]
##   piny <- par("pin")[2]
##   xscale <- (xmax - xmin)/pinx
##   box <- size * par("cin")[1]
##   if (box == 0) 
##     xbh <- xscale * 0.2
##   else xbh <- xscale * box/2
##   chr <- cex * par("cin")[2]
##   tail <- box + chr
##   yscale <- (ymax - ymin)/(piny - tail)
##   ytail <- yscale * tail
##   if (box == 0) 
##     ybx <- yscale * 0.2
##   else ybx <- yscale * box
##   ychr <- yscale * chr
##   ymin <- ymin - ytail
##   xf <- 0.1 * (xmax - xmin)
##   yf <- 0.1 * (ymax - ymin)
##   x1 <- xmin - xf
##   x2 <- xmax + xf
##   y1 <- ymin - yf
##   y2 <- ymax + yf
##   par(usr = c(x1, x2, y1, y2))
##     v <- as.character(tframe$var[1])
##     if (rptree) {
##         sp <- tree$splits[1, ]
##         val <- sp["index"]
##         if (sp["ncat"] > 1) {
##             r <- sp["index"]
##             string <- "attributes(tree)$xlevels$"
##             string <- paste(string, v, sep = "")
##             xl <- eval(parse(text = string))
##             lf <- rf <- ""
##             for (k in 1:sp["ncat"]) if (tree$csplit[r, k] == 1) 
##                 lf <- paste(lf, xl[k], sep = ",")
##             else rf <- paste(rf, xl[k], sep = ",")
##             if (!print.levels) 
##                 string <- v
##             else if (nchar(lf) + nchar(rf) > 30) # Avoid too long
##               string <- v
##             else
##               string <- paste(lf, "=", v, "=", rf)
            
##         }
##         else {
##             if (sp["ncat"] < 0) 
##                 op <- "< - >"
##             else op <- "> - <"
##             string <- paste(v, op, round(val, decimals))
##         }
##     }
##     else {
##         val <- substring(as.character(tframe$splits[1, 1]), 2)
##         string <- paste(as.character(v), "< - >", round(val, decimals))
##     }
##     text.default(x[1], y[1], string, cex = cex)
##     if (nodeinfo)
##     {
##       n <- tframe$n[1]
##       if (rtree)
##       {
##         z <- round(tframe$yval[1], digits)
##         r <- pcor[1]
##         string <- paste(z, " ", units, "; ", n, " ", cases, 
##                         "; ", r, "%", sep = "")
##       }
##       else {
##             z <- attr(tree, "ylevels")[tframe$yval[1]]
##             r <- crate[1]
##             string <- paste(z, "; ", n, " ", cases, "; ", r, 
##                 "%", sep = "")
##         }
##         text.default(x[1], y[1] - ychr, string, cex = cex)
##     }
##   for (i in 2:nnodes)
##   {
##     ytop <- ychr * (as.integer(nodeinfo) + 1)
##     if (y[i] < y[i-1])
##     {
##       lines(c(x[i-1], x[i]), c(y[i-1] - ytop, y[i-1] - ytop))
##       lines(c(x[i], x[i]), c(y[i-1] - ytop, y[i] + ychr))
##     }
##     else
##     {
##       lines(c(x[parent[i]], x[i]), c(y[parent[i]] - ytop, y[parent[i]] - ytop))
##       lines(c(x[i], x[i]), c(y[parent[i]] - ytop, y[i] + ychr))
##     }
##     if (!leaves[i])
##     {
##       v <- as.character(tframe$var[i])
##       if (rptree)
##       {
##         k <- 1
##         for (j in 1:(i-1))
##         {
##           m <- tframe$ncompete[j]
##           if (m > 0) k <- k + m + 1
##           m <- tframe$nsurrogate[j]
##           if (m > 0) k <- k + m
##         }
##         sp <- tree$splits[k, ]
##         val <- sp["index"]
##         if (sp["ncat"] > 1) {
##           r <- sp["index"]
##           string <- "attributes(tree)$xlevels$"
##           string <- paste(string, v, sep = "")
##           xl <- eval(parse(text = string))
##           lf <- rf <- ""
##           for (k in 1:sp["ncat"])
##             if (tree$csplit[r, k] == 1) 
##               lf <- paste(lf, xl[k], sep = ",")
##             else rf <- paste(rf, xl[k], sep = ",")
##           if (!print.levels) 
##             string <- v
##           else if (nchar(lf) + nchar(rf) > 10) # Avoid too long
##             string <- v
##           else
##             string <- paste(lf, "=", v, "=", rf)
##         }
##         else {
##           if (sp["ncat"] < 0) 
##             op <- "< - >"
##           else op <- "> - <"
##           string <- paste(v, op, round(val, decimals))
##         }
##       }
##       else {
##         val <- substring(as.character(tframe$splits[i, 
##                                                     1]), 2)
##         string <- paste(as.character(v), "< - >", round(val, decimals))
##       }
##       text.default(x[i], y[i], string, cex = cex)
##       if (nodeinfo) {
##         n <- tframe$n[i]
##         if (rtree) {
##           z <- round(tframe$yval[i], digits)
##           r <- pcor[i]
##           string <- paste(z, " ", units, "; ", n, " ", 
##                           cases, "; ", r, "%", sep = "")
##         }
##         else {
##           z <- attr(tree, "ylevels")[tframe$yval[i]]
##           r <- crate[i]
##           string <- paste(z, "; ", n, " ", cases, "; ", 
##                           r, "%", sep = "")
##         }
##         text.default(x[i], y[i] - ychr, string, cex = cex)
##       }
##     }
##     else
##     {
##       if (box == 0)
##       {
##         lines(c(x[i], x[i]), c(y[i], y[i] + ychr))
##         lines(c(x[i] - xbh, x[i] + xbh), c(y[i], y[i]))
##       }
##       else
##       {
##         # points(x[i], y[i], pch = pch, cex = size, col = kol[x[i]])
##       }
##       if (rtree)
##       {
##         z <- round(tframe$yval[i], digits)
##         text.default(x[i], y[i] - ybx, paste(z, units, 
##                                              sep = " "), cex = cex)
##       }
##       else
##       {
##         z <- attr(tree, "ylevels")[tframe$yval[i]]
##         text.default(x[i], y[i] - ybx, z, cex = cex)
##       }
##       n <- tframe$n[i]
##       text.default(x[i], y[i] - ybx - ychr,
##                    paste(n, cases, sep = " "), cex=cex)
##       if (! rtree)
##         text.default(x[i], y[i] - 1.6*ybx - ychr,
##                      paste(crate[i], "%", sep = ""), cex=cex)
##       #paste(crate[i], "%/", proportions[i], sep = ""), cex = cex)
##       if (box != 0)
##       {
##         # ORIG text.default(x[i], y[i], as.character(x[i]), 
##         text.default(x[i], y[i], as.character(leafnode[x[i]]),
##                      cex = cex, col=kol[x[i]], font=2)
##       }    
##     }
##   }
##   if (nodeinfo) {
##     if (rtree) 
##       string <- paste("Total deviance explained =", sum(pcor), 
##                       "%")
##     else string <- paste("Total classified correct =", trate, 
##                          "%")
##     if (box == 0) 
##       text.default(mean(x), ymin - 3.5 * ychr, string, cex = 1.2 * 
##                    cex)
##     else text.default(mean(x), ymin - 2.2 * ybx, string, 
##                       cex = 1.2 * cex)
##   }
## }

## exportRpartTab <- function()
## {
##   # Make sure we have a model first!
  
##   if (is.null(crs$rpart))
##   {
##     errorDialog("No Tree model is available. Be sure to build",
##                 "the model before trying to export it! You will need",
##                 "to press the Execute button (F2) in order to build the",
##                 "model.")
##     return()
##   }

##   startLog("EXPORT RPART AS PMML")

##    save.name <- getExportSaveName("rpart")
##   if (is.null(save.name)) return(FALSE)
##   ext <- tolower(get.extension(save.name))

##   # We can't pass "\" in a filename to the parse command in MS/Windows
##   # so we have to run the save/write command separately, i.e., not
##   # inside the string that is being parsed.
  
##   pmml.cmd <- "pmml(crs$rpart)"

##   if (ext == "xml")
##   {
##     appendLog("Export a decision tree as PMML.",
##               sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
##     saveXML(eval(parse(text=pmml.cmd)), save.name)
##   }
##   else if (ext == "c")
##   {
##     appendLog("Export a decision tyree as C code for WebFocus.",
##               sprintf('cat(pmmltoc(toString(%s)), file="%s")', pmml.cmd, save.name))
##     cat(pmmltoc(toString(eval(parse(text=pmml.cmd)))), file=save.name)
##   }
          
##   setStatusBar("The", toupper(ext), "file", save.name, "has been written.")
## }
