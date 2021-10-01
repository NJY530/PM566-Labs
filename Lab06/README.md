Lab06
================
Jiayi Nie

## Read the data

``` r
fn <- "mtsamples.csv"

if(!file.exists(fn))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv", destfile = fn)

mtsamples <- read.csv(fn)
mtsamples <- as_tibble(mtsamples)
```

## Q1:How many and how are specialities distributed?

``` r
specialties <- mtsamples %>%
  count(medical_specialty)

specialties %>%
  arrange(desc(n)) %>%
  top_n(15) %>%
  knitr::kable()
```

    ## Selecting by n

| medical\_specialty            |    n |
| :---------------------------- | ---: |
| Surgery                       | 1103 |
| Consult - History and Phy.    |  516 |
| Cardiovascular / Pulmonary    |  372 |
| Orthopedic                    |  355 |
| Radiology                     |  273 |
| General Medicine              |  259 |
| Gastroenterology              |  230 |
| Neurology                     |  223 |
| SOAP / Chart / Progress Notes |  166 |
| Obstetrics / Gynecology       |  160 |
| Urology                       |  158 |
| Discharge Summary             |  108 |
| ENT - Otolaryngology          |   98 |
| Neurosurgery                  |   94 |
| Hematology - Oncology         |   90 |

There are 40 specialties. Let’s take a look at the distribution.

``` r
ggplot(mtsamples, aes(medical_specialty))+
  geom_histogram(stat="count") +
  coord_flip()
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

these are not evenlt (uniformly) distributed.

## Q2

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  top_n(20)%>%
  ggplot(aes(n,fct_reorder(word,n)))+
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/Tokenize%20the%20the%20words%20in%20the%20transcription%20column-1.png)<!-- -->

The word “patient” is seems to be important, but we observe a lot of
stopwords.

## Q3

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words, by= "word")%>%
  top_n(20)%>%
  ggplot(aes(n,fct_reorder(word,n)))+
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/Redo%20visualization%20but%20remove%20stopwords%20before-1.png)<!-- -->
