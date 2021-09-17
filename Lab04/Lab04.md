Lab 03
================

\#\#1.Read the data

``` r
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("met_all.gz")
```

\#\#2.Prepare the data

``` r
#Remove temperatures less than -17C
met <- met[temp>=-17]

#Make sure there are no missing data in the key variables coded as 9999, 999, etc
met[,table(is.na(temp))]
```

    ## 
    ##   FALSE 
    ## 2317212
