
[![Travis-CI Build Status](https://travis-ci.org/rdinter/nassR.svg?branch=master)](https://travis-ci.org/rdinter/nassR)

An alternative for downloading various USDA data from <https://quickstats.nass.usda.gov/> through R. You must sign up for an [API token](https://quickstats.nass.usda.gov/api) from the mentioned website in order for this package to work.

Install
=======

Development version (needs devtools installed):

``` r
devtools::install_github("rdinter/nassR", upgrade = "never")
```

Basic Usage
===========

If a query works on the <https://quickstats.nass.usda.gov/> interface, then it will work with the `nassR` package.

There are three basic functions for this package with the first as the workhorse:

1.  `nass_data` this will return a data.frame to the specifications of the query from all of the arguments set in the function call. This mimics the simple "GET DATA" command off of <https://quickstats.nass.usda.gov/> and requires an API token. There is a 50,000 limit for each call.
2.  `nass_param` returns all of the possible values for a parameter in a query. Helpful to understand how to subset a query if it runs into the 50,000 limit.
3.  `nass_count` returns the number of records for a query. Very useful in conjunction with `nass_param` to determine what queries can return data with a `nass_data` call.

Key Install
-----------

``` r
library(nassR)
set_nass_key("YOUR_KEY_IN_QUOTATIONS")
# First time, reload your enviornment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("NASS_KEY")
```

The above script will add a line to your `.Renviron` file to be re-used whenever you are using the package. If you are not comfortable with that, you can add the following line to your `.Renviron` file manually to produce the same result.

`NASS_KEY='YOUR_KEY_IN_SINGLE_QUOTES'`

Sample Code
-----------

### Example 1

Find the number of observations for county level variables in Wake County North Carolina.

``` r
nass_count(state_name = "NORTH CAROLINA", county_name = "WAKE")
```

    ## [1] 7012

    [1] 6997

### Example 2

View all of the parameter values for the group\_desc category when the sector is CROPS.

``` r
nass_param("group_desc", sector_desc = "CROPS")
```

    ## [1] "CROP TOTALS"       "FIELD CROPS"       "FRUIT & TREE NUTS"
    ## [4] "HORTICULTURE"      "VEGETABLES"

    [1] "CROP TOTALS"       "FIELD CROPS"       "FRUIT & TREE NUTS" "HORTICULTURE"      "VEGETABLES"       

### Example 3

Download the data pertaining to Wake County in 2012 for its value of agricultural land. Then look at the structure of the object returned by the function call.

``` r
j5 <- nass_data(year = 2012, agg_level_desc = "COUNTY", short_desc = "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $", county_name = "WAKE")
str(j5)
```

    ## 'data.frame':    1 obs. of  39 variables:
    ##  $ week_ending          : chr ""
    ##  $ state_name           : chr "NORTH CAROLINA"
    ##  $ country_code         : chr "9000"
    ##  $ location_desc        : chr "NORTH CAROLINA, CENTRAL PIEDMONT, WAKE"
    ##  $ begin_code           : chr "12"
    ##  $ zip_5                : chr ""
    ##  $ county_ansi          : chr "183"
    ##  $ state_alpha          : chr "NC"
    ##  $ util_practice_desc   : chr "ALL UTILIZATION PRACTICES"
    ##  $ domain_desc          : chr "TOTAL"
    ##  $ asd_desc             : chr "CENTRAL PIEDMONT"
    ##  $ freq_desc            : chr "POINT IN TIME"
    ##  $ prodn_practice_desc  : chr "ALL PRODUCTION PRACTICES"
    ##  $ end_code             : chr "12"
    ##  $ sector_desc          : chr "ECONOMICS"
    ##  $ short_desc           : chr "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"
    ##  $ country_name         : chr "UNITED STATES"
    ##  $ Value                : chr "806,464,000"
    ##  $ reference_period_desc: chr "END OF DEC"
    ##  $ CV (%)               : chr "7.9"
    ##  $ class_desc           : chr "INCL BUILDINGS"
    ##  $ asd_code             : chr "50"
    ##  $ agg_level_desc       : chr "COUNTY"
    ##  $ county_name          : chr "WAKE"
    ##  $ region_desc          : chr ""
    ##  $ watershed_desc       : chr ""
    ##  $ state_ansi           : chr "37"
    ##  $ congr_district_code  : chr ""
    ##  $ domaincat_desc       : chr "NOT SPECIFIED"
    ##  $ state_fips_code      : chr "37"
    ##  $ group_desc           : chr "FARMS & LAND & ASSETS"
    ##  $ watershed_code       : chr "00000000"
    ##  $ unit_desc            : chr "$"
    ##  $ source_desc          : chr "CENSUS"
    ##  $ load_time            : chr "2012-12-31 00:00:00"
    ##  $ county_code          : chr "183"
    ##  $ statisticcat_desc    : chr "ASSET VALUE"
    ##  $ commodity_desc       : chr "AG LAND"
    ##  $ year                 : chr "2012"

    'data.frame':   1 obs. of  39 variables:
     $ week_ending          : chr ""
     $ state_name           : chr "NORTH CAROLINA"
     $ country_code         : chr "9000"
     $ location_desc        : chr "NORTH CAROLINA, CENTRAL PIEDMONT, WAKE"
     $ begin_code           : chr "12"
     $ zip_5                : chr ""
     $ county_ansi          : chr "183"
     $ state_alpha          : chr "NC"
     $ util_practice_desc   : chr "ALL UTILIZATION PRACTICES"
     $ domain_desc          : chr "TOTAL"
     $ asd_desc             : chr "CENTRAL PIEDMONT"
     $ freq_desc            : chr "POINT IN TIME"
     $ prodn_practice_desc  : chr "ALL PRODUCTION PRACTICES"
     $ end_code             : chr "12"
     $ sector_desc          : chr "ECONOMICS"
     $ short_desc           : chr "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"
     $ country_name         : chr "UNITED STATES"
     $ Value                : chr "806,464,000"
     $ reference_period_desc: chr "END OF DEC"
     $ CV (%)               : chr "7.9"
     $ class_desc           : chr "INCL BUILDINGS"
     $ asd_code             : chr "50"
     $ agg_level_desc       : chr "COUNTY"
     $ county_name          : chr "WAKE"
     $ region_desc          : chr ""
     $ watershed_desc       : chr ""
     $ state_ansi           : chr "37"
     $ congr_district_code  : chr ""
     $ domaincat_desc       : chr "NOT SPECIFIED"
     $ state_fips_code      : chr "37"
     $ group_desc           : chr "FARMS & LAND & ASSETS"
     $ watershed_code       : chr "00000000"
     $ unit_desc            : chr "$"
     $ source_desc          : chr "CENSUS"
     $ load_time            : chr "2012-12-31 00:00:00"
     $ county_code          : chr "183"
     $ statisticcat_desc    : chr "ASSET VALUE"
     $ commodity_desc       : chr "AG LAND"
     $ year                 : chr "2012"
