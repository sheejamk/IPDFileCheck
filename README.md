# IPDFileCheck
<!-- badges: start -->
   [![Build Status](https://app.travis-ci.com/sheejamk/IPDFileCheck.svg?branch=master)](https://app.travis-ci.com/github/sheejamk/IPDFileCheck)
<!-- badges: end -->
 
<!-- badges: start -->
[![codecov](https://codecov.io/gh/sheejamk/ipdFileCheck/branch/main/graph/badge.svg?token=PPLl5IBhyy)](https://app.codecov.io/gh/sheejamk/ipdFileCheck)
<!-- badges: end -->

IPDFileCheck is a package that can be used to check the data file from a randomised clinical trial (RCT). The standard checks on data file from RCT will be of the following
1. To check the file exists and readable
2. To check if the column exists, 
3. To get the column number if the column name is known,
4. To test column contents ie do they contain specific items in a given list?
5. To test column names of a data being different from what specified,
6. To check the format of column 'age' in data
7. To check the format of column 'gender' in data
8. To check the format of column contents -numeric or string
9. To return the column number if the pattern is contained in the colnames of a data
10. To return descriptive statistics, sum, no of observations, mean, mode. median, range, standard deviation and standard error
11. To present the mean and sd of a data set in the form Mean (SD)
12. To return a subgroup when certain variable equals the given value while omitting those with NA
13. To find the number and percentages of categories
14. To calculate age from date of birth and year of birth
There are some other helper functions included.
**Usage**<br/>
See the User Guide Vignette for examples on using the package 
**Installation**<br/>
Latest release can be installed from www.github.com/sheejamk/ipdfilecheck
