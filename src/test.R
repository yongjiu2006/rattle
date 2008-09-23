# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-09-23 21:21:28 Graham Williams>
#
# Test Tab
#
# Copyright (c) 2008 Togaware Pty Ltd
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
# Callbacks

on_test_groupby_checkbutton_toggled<- function(button)
{
  if (button$getActive())
  {
    #theWidget("test_groupby_combobox")$setSensitive(TRUE)
    theWidget("test_groupby_target_label")$setSensitive(TRUE)
    theWidget("test_vars2_label")$setSensitive(FALSE)
    theWidget("test_vars2_combobox")$setSensitive(FALSE)
  }
  else
  {
    #theWidget("test_groupby_combobox")$setSensitive(FALSE)
    theWidget("test_groupby_target_label")$setSensitive(FALSE)
    theWidget("test_vars2_label")$setSensitive(TRUE)
    theWidget("test_vars2_combobox")$setSensitive(TRUE)
  }
}

########################################################################
# Functionality

resetTestTab <- function(new.dataset=TRUE)
{
  cbox1 <- theWidget("test_vars1_combobox")
  cbox2 <- theWidget("test_vars2_combobox")

  if (new.dataset)
  {
    # 080921 Set up the list of numeric variables to choose from as
    # sample 1 and, optionally,  sample 2.
    
    vl <- colnames(crs$dataset)[getNumericVariables("indicies")]

    if (not.null(vl))
    {
      cbox1$getModel()$clear()
      lapply(vl, cbox1$appendText)
      cbox2$getModel()$clear()
      lapply(vl, cbox2$appendText)
    }
  }

  # If there is a target variable, and it is binary, then enable Group
  # By Target and set the target label appropriately.

  if (is.null(crs$target))
  {
    theWidget("test_groupby_checkbutton")$setSensitive(FALSE)
    theWidget("test_groupby_checkbutton")$setActive(FALSE)
    theWidget("test_groupby_target_label")$setSensitive(FALSE)
  }
  else
  {
    theWidget("test_groupby_checkbutton")$setSensitive(TRUE)
    theWidget("test_groupby_checkbutton")$setActive(TRUE)
    theWidget("test_groupby_target_label")$setSensitive(TRUE)
    theWidget("test_groupby_target_label")$setText(crs$target)
  }
}

executeTestTab <- function()
{

  TV <- "test_textview"

  # Obtain interface information.
  
  if (theWidget("test_groupby_checkbutton")$getActive())
  {
    v1 <- v2 <- theWidget("test_vars1_combobox")$getActiveText()
    if (is.null(v1))
    {
      errorDialog("Please first choose a column from which the sample",
                  "will be obtained.")
      return(FALSE)
    }
    lvl <- levels(as.factor(crs$dataset[[crs$target]]))
    s1 <- sprintf('[crs$dataset[["%s"]] == %s,]', crs$target, lvl[1])
    s2 <- sprintf('[crs$dataset[["%s"]] == %s,]', crs$target, lvl[2])
    msg <- sprintf(paste('come from the \n"%s" column, grouped by "%s",',
                         'with\nvalues "%s" and "%s"'),
                   v1, crs$target, lvl[1], lvl[2])
  }
  else
  {
    v1 <- theWidget("test_vars1_combobox")$getActiveText()
    if (is.null(v1))
    {
      errorDialog("Please first choose a column from which the first sample",
                  "will be obtained.")
      return(FALSE)
    }
    v2 <- theWidget("test_vars2_combobox")$getActiveText()
    if (is.null(v2))
    {
      errorDialog("Please first choose a column from which the second sample",
                  "will be obtained.")
      return(FALSE)
    }
    s1 <- s2 <- ""
    msg <- sprintf('are the two\ncolumns, "%s" and "%s"', v1, v2)
  }
  msg <- sprintf("\nThe two samples being compared %s.\n", msg)
  

  # Start the log for this task.

  startLog("PERFORM TEST")

  # Ensure the package is available.

  lib.cmd <- "require(fBasics, quietly=TRUE)"
  if (! packageIsAvailable("fBasics", "location t-test")) return(FALSE)
  appendLog("Use the fBasics package for tests.", lib.cmd)
  eval(parse(text=lib.cmd))

  resetTextview(TV)

  test <- NULL
  preamble <- NULL
  options <- ""
  
  if (theWidget("test_distr_radiobutton")$getActive())
  {
    test <- "ks2Test"
    preamble <- paste("The Kolmogorov-Smirnov test indicates whether the two",
                      "samples are similarly distributed.\n", sep="\n")
  }
  else if (theWidget("test_ttest_radiobutton")$getActive())
  {
    test <- "locationTest"
    preamble <- paste("The t Test is performed on the two samples to test the",
                      "hypothesis that the difference between the two",
                      "means is zero. It is assumed the two samples are normally",
                      "distriubted. Otherwise use the Kruskal-Wallis test.",
                      "\nThe confidence interval is an interval around",
                      "the expected difference between the means.",
                      "\nA low p-value (less than 0.05) indicates statistically",
                      "significant results.",
                      "\nTwo variants are reported: assume equal and unequal variances.",
                      "\n",
                      sep="\n")
  }
  else if (theWidget("test_kw_radiobutton")$getActive())
  {
    test <- "locationTest"
    options <- ', method="kw"'
    preamble <- paste("The Kruskal-Wallis test is performed on the two samples",
                      "to test the hypothesis that the difference between the two",
                      "means is zero. It does not assume that the two samples",
                      "are normally distriubted.",
                      "\nThe confidence interval is an interval around",
                      "the expected difference between the means.\n",
                      sep="\n")
  }
  else if (theWidget("test_variance_radiobutton")$getActive())
    test <- "varianceTest"
  else if (theWidget("test_correlation_radiobutton")$getActive())
    test <- "correlationTest"
  
  test.cmd <- sprintf(paste('%s(na.omit(crs$dataset%s[["%s"]]),',
                            'na.omit(crs$dataset%s[["%s"]])%s)'),
                      test, s1, v1, s2, v2, options)
  appendLog("Perform the test.", test.cmd)
  resetTextview(TV, preamble, msg, collectOutput(test.cmd))

  setStatusBar("Test completed.")
}

  
