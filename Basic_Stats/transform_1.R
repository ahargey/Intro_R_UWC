library(tidyverse)

chick <- datasets::ChickWeight

Lets say we want to transform the chickweight dataset
We are transforming becausee it is not normally distributed
Now we transforming the weight column within the dataset as
this column appears to be non normally distributed

chicks_transformed <- chick %>% 
  mutate(log10 = log10(weight),
         sqr = sqrt(weight),
         log = log(weight))

What are regressions and what are correlations
Correlations
when we are comparing variables within the same sample
There are three types of correlations

1. Pearson correlation
2. Spearmans correlation
3. Kendal Rank correlation

  Pearson correlation: When the values we are comparing are continuous
  Spearmans rank correlation: When the data we want to compare are not continuous, but rather ordinal
  Kendal rank correlation: This test will work for both continuous and ordinal data.

Often many people would just prefer to use the kendal rank correlation because it works 
on any type of dataset and may be easier at times
Avoids all the assumptions made

Regressions
Regressions test the statistical significance of the *dependence* of one continuous
variable on one or many independent continuous variables.
  
Messy data and clearing up the different datasets
  Data should be longer then it is wide
  Each variable need its own column
  One value per cell

I know there is multiple test that we can run to test for normality, does it matter which one we use?
  Nope it does not matter, But if you run one test and you see that there is one column that may not be normal
  then it is wise to do one of the transfomrations above
  By transforming your data one automatically considers it as normal so if you are unsure then transform it either way

To answer this fundamental question one often uses a *t*-test.
There are several variations of *t*-tests, depending on the nature of our samples and the type of question being asked:
  * **One-sample *t*-tests:** only one sample set of data that we wish to compare against a known population mean:
  * one-sided one-sample *t*-tests
* two-sided one-sample *t*-tests
* **Two-sample *t*-tests:** the means of two groups are compared against each other:
  * independent sample *t*-tests
* one-sided two-sample *t*-tests
* two-sided two-sample *t*-tests
* paired sample *t*-tests
* one-sided
* two-sided

Before we cover each of these, we need to understand some of the assumptions behind *t*-tests
Need to test the assumptions
* the dependent variable must be continuous (i.e. it is measured at the interval or ratio level),
* the observations in the groups being compared are independent of each other,
* the data are **normally distributed**, and
* that the data are **homoscedastic**, and in particular, that there are no outliers.


One sample t-test
  Only one sample set of data that we wish to compare against a known population mean

One-sided one-sample *t*-tests
  If the value is less than the mean or if the value is more than the mean

Two-sample *t*-tests
  A two-sample *t*-test is used when we have samples from two different populations that we would like to compare against one another.







