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
  filter(!grepl("^[0-9]+$",x=word)) %>%
  top_n(20)%>%
  ggplot(aes(n,fct_reorder(word,n)))+
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/Redo%20visualization%20but%20remove%20stopwords%20before-1.png)<!-- -->

## Q4

``` r
mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n=2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(20)%>%
  ggplot(aes(n,fct_reorder(bigram,n)))+
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
mtsamples %>%
  unnest_ngrams(output = trigram, input = transcription, n=3) %>%
  count(trigram, sort = TRUE) %>%
  top_n(20)%>%
  ggplot(aes(n,fct_reorder(trigram,n)))+
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

Now some phrases start to show up. e.g. “tolerated the procedure”

## Q5: Pick a word and count the words that appears after and before it.

``` r
bigrams <- mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n=2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter((word1 =="patient") | (word2 =="patient"))

bigrams %>%
  filter(word1 == "patient")%>%
  select(word1,word2) %>%
  count(word2, sort = TRUE)
```

    ## # A tibble: 588 × 2
    ##    word2         n
    ##    <chr>     <int>
    ##  1 was        6293
    ##  2 is         3332
    ##  3 has        1417
    ##  4 tolerated   994
    ##  5 had         888
    ##  6 will        616
    ##  7 denies      552
    ##  8 and         377
    ##  9 states      363
    ## 10 does        334
    ## # … with 578 more rows

``` r
bigrams %>%
  filter(word2 == "patient")%>%
  select(word1,word2)%>%
  count(word1, sort = TRUE)
```

    ## # A tibble: 269 × 2
    ##    word1         n
    ##    <chr>     <int>
    ##  1 the       20307
    ##  2 this        470
    ##  3 history     101
    ##  4 a            67
    ##  5 and          47
    ##  6 procedure    32
    ##  7 female       26
    ##  8 with         25
    ##  9 use          24
    ## 10 old          23
    ## # … with 259 more rows

Since we are looking ar single word again, it’s good idea to remove the
stopwords and numbers.

``` r
bigrams %>%
  filter(word1 == "patient") %>%
  filter(!(word2 %in% stop_words$word) & !grepl("^[0-9]+$",word2))%>%
  count(word2, sort = TRUE)%>%
  top_n(10)%>%
  knitr::kable()
```

    ## Selecting by n

| word2      |   n |
| :--------- | --: |
| tolerated  | 994 |
| denies     | 552 |
| underwent  | 180 |
| received   | 160 |
| reports    | 155 |
| understood | 113 |
| lives      |  81 |
| admits     |  69 |
| appears    |  68 |
| including  |  67 |

``` r
bigrams %>%
  filter(word2 == "patient") %>%
  filter(!(word1 %in% stop_words$word) & !grepl("^[0-9]+$",word1))%>%
  count(word1, sort = TRUE)%>%
  top_n(10)%>%
  knitr::kable()
```

    ## Selecting by n

| word1       |   n |
| :---------- | --: |
| history     | 101 |
| procedure   |  32 |
| female      |  26 |
| sample      |  23 |
| male        |  22 |
| illness     |  16 |
| plan        |  16 |
| indications |  15 |
| allergies   |  14 |
| correct     |  11 |
| detail      |  11 |
