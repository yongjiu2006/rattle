# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-10-21 18:39:26 Graham Williams>
#
# Test Tab
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
# Callbacks

# 090205 Now leave all Test radio buttons active, always. The user
# selects one and the Group By option may disappear if they select one
# of the Paired tests. This is more logical than using the Group By
# option to toggle the sensitivity of the Paired tests.

on_test_correlation_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    theWidget("test_groupby_checkbutton")$setActive(FALSE)
    theWidget("test_groupby_checkbutton")$setSensitive(FALSE)
    theWidget("test_groupby_target_label")$setSensitive(FALSE)
  }
  else
  {
    theWidget("test_groupby_checkbutton")$setSensitive(TRUE)
    theWidget("test_groupby_target_label")$setSensitive(TRUE)
  }
}

on_test_wilcoxon_signed_radiobutton_toggled <- on_test_correlation_radiobutton_toggled


on_test_groupby_checkbutton_toggled<- function(button)
{
  # 090205 Only deal with enabling/disabling the var2 box on this bing
  # toggled.
  
  if (button$getActive())
  {
    theWidget("test_vars2_label")$setSensitive(FALSE)
    theWidget("test_vars2_combobox")$setSensitive(FALSE)
  }
  else
  {
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

  if (noDatasetLoaded()) return(FALSE)
  
  # Obtain interface information.
  
  if (theWidget("test_groupby_checkbutton")$getActive())
  {
    v1 <- v2 <- theWidget("test_vars1_combobox")$getActiveText()
    if (is.null(v1))
    {
      errorDialog("Please first choose a variable from which the sample",
                  "will be obtained.")
      return(FALSE)
    }
    if (theWidget("test_correlation_radiobutton")$getActive()
        || theWidget("test_wilcoxon_signed_radiobutton")$getActive())
    {
      errorDialog("The Correlation and Wilcoxon Signed Rank tests",
                  "can only be applied to paired populations.",
                  "The Group By option is not likely to give paired populations",
                  "(requiring the same number of observations in each sample).",
                  "Please de-select Group By and choose",
                  "two variables that represent observations of the entity at",
                  "two different times. Alternatively, choose a two sample",
                  "(non-paired) test if that is appropriate.")
      return(FALSE)
    }
    lvl <- levels(as.factor(crs$dataset[[crs$target]]))
    s1 <- sprintf('crs$dataset[["%s"]] == "%s"', crs$target, lvl[1])
    s2 <- sprintf('crs$dataset[["%s"]] == "%s"', crs$target, lvl[2])
    msg <- sprintf(paste('come from the "%s" variable, grouped by \n"%s",',
                         'with values "%s" and "%s"'),
                   v1, crs$target, lvl[1], lvl[2])
  }
  else
  {
    v1 <- theWidget("test_vars1_combobox")$getActiveText()
    if (is.null(v1))
    {
      errorDialog("Please first choose a variable from which the first sample",
                  "will be obtained.")
      return(FALSE)
    }
    v2 <- theWidget("test_vars2_combobox")$getActiveText()
    if (is.null(v2))
    {
      errorDialog("Please first choose a variable from which the second sample",
                  "will be obtained.")
      return(FALSE)
    }
    s1 <- s2 <- ""
    msg <- sprintf('are the two variables, "%s" and "%s"', v1, v2)
  }
  msg <- sprintf("\nThe two samples being compared %s.\n", msg)
  

  # Start the log for this task.

  startLog("Perform Test")

  # Ensure the package is available.

  lib.cmd <- "require(fBasics, quietly=TRUE)"
  if (! packageIsAvailable("fBasics", "location T-Test")) return(FALSE)
  appendLog("Use the fBasics package for tests.", lib.cmd)
  eval(parse(text=lib.cmd))

  resetTextview(TV)

  test <- NULL
  preamble <- NULL
  options <- ""
  
  if (theWidget("test_distr_radiobutton")$getActive())
  {
    test <- "ks2Test"
    preamble <- "KOLMOGOROV-SMIRNOV TEST

The Kolmogorov-Smirnov test is a non-parametric test of the
similarity of two distributions. The null hypothesis is that the
two samples are drawn from the same distribution. The two-sided and
the two one-sided tests are performed.

The STATISTIC calculated is the so called D statsitic.
For similar distributions the statistic converges to zero.

If the p-value is less than 0.05 then we reject the null hypothesis and
accpet the alternative hypothesis, that the distributions differ, at the
95% level of confidence.
"
  }
  else if (theWidget("test_ttest_radiobutton")$getActive())
  {
    test <- "locationTest"
    preamble <- "TWO-SAMPLE T-TEST

The two-sample T-test is performed on the two specified samples. The
null hypothesis is that the difference between the two means is zero.

This test assumes that the two samples are normally distributed. If not,
use the Wilcoxon Rank-Sum test.

The confidence interval is an interval around the expected difference
between the means.

If the p-value is less than 0.05 then we reject the null hypothesis and
accpet the alternative hypothesis, that the means differ, at the 95% level
of confidence.

Two variants of the test are reported: for equal and unequal variances.
"
  }
  else if (theWidget("test_kw_radiobutton")$getActive())
  {
    test <- "locationTest"
    options <- ', method="kw"'
    preamble <- paste("KRUSKAL-WALLIS TEST

The Kruskal-Wallis test is performed on the two samples",
                      "to test the hypothesis that the difference between the two",
                      "means is zero. It does not assume that the two samples",
                      "are normally distributed.",
                      "\nThe confidence interval is an interval around",
                      "the expected difference between the means.\n",
                      sep="\n")
  }
  else if (theWidget("test_wilcoxon_radiobutton")$getActive())
  {
    test <- "wilcox.test"
    # options <- ", conf.int=TRUE" Could do this but needs more explanation
    preamble <- "WILCOXON RANK SUM TEST

The two-sample non-parametric Wilcoxon rank sum test (equivalent to
the Mann-Whitney test) is performed on the two specified samples. The null
hypothesis is that the distributions are the same (i.e., there is no
shift in the location of the two distributions) with an alternative
hypothesis that they differ on location (based on median).

This test does not assume that the two samples are normally distributed
but does assume they have distributions of the same shape.

If the p-value is less than 0.05 then we reject the null hypothesis and
accept the alternative hypothesis, that the two samples have different medians,
at the 95% level of confidence.
"
  }
  else if (theWidget("test_wilcoxon_signed_radiobutton")$getActive())
  {
    test <- "wilcox.test"
    options <- ", paired=TRUE"
    if (! is.null(union(attr(na.omit(crs$dataset[,v1]), "na.action"),
                        attr(na.omit(crs$dataset[,v2]), "na.action"))))
    {
      test <- paste('miss <- union(',
                    'attr(na.omit(crs$dataset[,"', v1, '"]), "na.action"), ',
                    'attr(na.omit(crs$dataset[,"', v2, '"]), "na.action"))\n',
                    test, sep="")
      s1 <- s2 <- "-miss"
    }
    preamble <- "WILCOXON SIGNED RANK TEST

The paired sample non-parametric Wilcoxon signed rank test is
performed on the two specified samples. The two samples are expected to be
paired (two observations for the same entity). The null hypothesis is that
the distributions are the same.

This test does not assume that the two samples are are normally distributed.

If the p-value is less than 0.05 then we reject the null hypothesis and
accept the alternative hypothesis, that the distributions differ, at the
95% level of confidence.
"
  }
  else if (theWidget("test_variance_radiobutton")$getActive())
  {
    test <- "varianceTest"
    preamble <- "TWO SAMPLE F-TEST

The two sample F-test is performed on the two specified samples. The
null hypothesis is that the ratio of the variances of the populations from
which they were drawn is equal to one.

This test assumes that the two samples are normally distributed.

If the p-value is less than 0.05 then we reject the null hypothesis and
accept the alternative hypothesis, that the two samples have different
variances, at the 95% level of confidence.
"
  }
  else if (theWidget("test_correlation_radiobutton")$getActive())
  {
    test <- "correlationTest"

    if (! is.null(union(attr(na.omit(crs$dataset[,v1]), "na.action"),
                        attr(na.omit(crs$dataset[,v2]), "na.action"))))
    {
      test <- paste('miss <- union(',
                    'attr(na.omit(crs$dataset[,"', v1, '"]), "na.action"), ',
                    'attr(na.omit(crs$dataset[,"', v2, '"]), "na.action"))\n',
                    test, sep="")
      s1 <- s2 <- "-miss"
    }

    preamble <- "CORRELATION TEST

The paired sample correlation test is performed on the two specified samples.
The two samples are expected to be paired (two observations for the same entity).
The null hypothesis is that the two samples have no (i.e., 0) correlation.
Pearson's product moment correlation coefficient is used.

If the p-value is less than 0.05 then we reject the null hypothesis and
accept the alternative hypothesis that the samples are correlated,
at the 95% level of confidence.
"
  }
  
#  test.cmd <- sprintf(paste('%s(na.omit(crs$dataset%s[["%s"]]),',
#                            'na.omit(crs$dataset%s[["%s"]])%s)'),
#                      test, s1, v1, s2, v2, options)
  test.cmd <- sprintf(paste('%s(na.omit(crs$dataset[%s, "%s"]),',
                            'na.omit(crs$dataset[%s, "%s"])%s)'),
                      test, s1, v1, s2, v2, options)
  appendLog("Perform the test.", test.cmd)
  resetTextview(TV, preamble, msg, collectOutput(test.cmd))

  setStatusBar("Test completed.")
}

  
