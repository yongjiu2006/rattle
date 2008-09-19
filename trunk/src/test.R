# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-09-19 11:15:08 Graham Williams>
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

resetTestTab <- function(new.dataset=TRUE)
{
  cbox1 <- theWidget("test_vars1_combobox")
  cbox2 <- theWidget("test_vars2_combobox")

  if (new.dataset)
  {
    vl <- colnames(crs$dataset)[getNumericVariables("indicies")]

    if (not.null(vl))
    {
      cbox1$getModel()$clear()
      lapply(vl, cbox1$appendText)
      cbox2$getModel()$clear()
      lapply(vl, cbox2$appendText)
    }
  }
}

executeTestTab <- function()
{

  TV <- "test_textview"

  # Obtain interface information.
  
  v1 <- theWidget("test_vars1_combobox")$getActiveText()
  v2 <- theWidget("test_vars2_combobox")$getActiveText()

  startLog("PERFORM TEST")

  lib.cmd <- "require(fBasics, quietly=TRUE)"
  if (! packageIsAvailable("fBasics", "location t-test")) return(FALSE)
  appendLog("Use the fBasics package for tests.", lib.cmd)
  eval(parse(text=lib.cmd))

  resetTextview(TV)

  test <- NULL
  preamble <- NULL
  
  if (theWidget("test_distr_radiobutton")$getActive())
  {
    test <- "ks2Test"
    preamble <- paste("The results of testing whether the two datasets",
                      "are similarly distributed.")
  }
  else if (theWidget("test_ttest_radiobutton")$getActive())
  {
    test <- "locationTest"
    preamble <- paste("The t Test is performed on the two samples to test the",
                      "hypothesis that the difference between the two",
                      "means is zero.",
                      "\nThe confidence interval is an interval around",
                      "the expected difference between the means.\n\n",
                      sep="\n")
  }
  else if (theWidget("test_variance_radiobutton")$getActive())
    test <- "varianceTest"
  else if (theWidget("test_correlation_radiobutton")$getActive())
    test <- "correlationTest"
  
  test.cmd <- sprintf(paste('%s(na.omit(crs$dataset[["%s"]]),',
                            'na.omit(crs$dataset[["%s"]]))'),
                      test, v1, v2)
  appendLog("Perform the test.", test.cmd)
  resetTextview(TV, preamble, collectOutput(test.cmd))

  setStatusBar("Test performed.")
}

  
