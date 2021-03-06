# Publishing Star - How to make the data speak
## Data Extraction from BigQuery(SQL)


``` sql
SELECT * 
FROM [gdelt-bq:full.events] 
WHERE sourceurl LIKE '%bloomberg.com%'; -- filter by source url

SELECT * 
FROM [gdelt-bq:full.events] 
WHERE sourceurl LIKE '%www.cosmopolitan.com%'; -- there is a Canandian website called agrocosmopolitan...
```
The results from the query is too big(10,000 results limit) to be export directly from BigQuery's web UI, therefore, we need to create a table to store the results and export it to cloud and then download. Follows are the Google documentations for these steps.

[Create Table in BigQuery](https://cloud.google.com/bigquery/docs/tables) <br>
[Creat a bucket in Google Cloud](https://cloud.google.com/storage/docs/creating-buckets)<br>
[Export Table to Google Cloud](https://cloud.google.com/bigquery/docs/exporting-data)

## Data Wrangeling(R)
```{r}
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(readr)
  library(urltools)
  library(tm)
  library(wordcloud2)
  library(RColorBrewer)
  library(tidytext)
})
```

```r
wsj <- read.csv("wsj.csv")
bloomberg <- read.csv("bloomberg.csv")
nytimes <- read.csv("nytimes.csv")
washingtonpost <- read.csv("washingtonpost.csv")

newyorker <- read.csv("newyorker.csv")
vogue <- read.csv("vogue.csv")
cosmopolitan <- read.csv("cosmopolitan.csv")

washingtonpost <- washingtonpost %>%
  select(-X) %>%
  mutate(publisher = "WashingtonPost")

bloomberg <- bloomberg %>% 
  mutate(publisher = "Bloomberg")

wsj <- wsj %>% 
  mutate(publisher = "WSJ")

nytimes <- nytimes %>% 
  mutate(publisher = "NYtimes")

newyorker <- newyorker %>% 
  mutate(publisher = "NewYorker")

vogue <- vogue %>% 
  mutate(publisher = "Vogue")

cosmopolitan <- cosmopolitan %>% 
  mutate(publisher = "Cosmopolitan")

events <- rbind(washingtonpost,bloomberg) %>%
  rbind(wsj) %>%
  rbind(nytimes) %>%
  rbind(newyorker) %>%
  rbind(vogue) %>%
  rbind(cosmopolitan) %>%  
  filter( Year > 2012)

title <- events %>%
  select(GLOBALEVENTID,SQLDATE,publisher,SOURCEURL) 

#write.csv(events,'events.csv')
#write.csv(washingtonpost,'washingtonpost.csv')
write_rds(title,"title.rda")
write_rds(events,'events.rda')
```
### Text Processing
Parse url to extrat the article titles
```r
parsed_address <- url_parse(title$SOURCEURL)

title$path  <- parsed_address$path

title <- title %>%
  separate(path,into = c("a","b","c","d","e","f","g","h","i","j"), sep = "/", 
            remove = FALSE, extra = "merge", fill = "right") %>%
  mutate(title = ifelse((publisher != "WashingtonPost") & grepl("-",j),j,
                 ifelse((publisher != "WashingtonPost") & grepl("-",i),i,
                 ifelse((publisher != "WashingtonPost") & grepl("-",h),h,
                 ifelse((publisher != "WashingtonPost") & grepl("-",g),g,
                 ifelse((publisher != "WashingtonPost") & grepl("-",f),f,
                 ifelse((publisher != "WashingtonPost") & grepl("-",e),e,
                 ifelse((publisher != "WashingtonPost") & grepl("-",d),d,
                 ifelse((publisher != "WashingtonPost") & grepl("-",c),c,
                 ifelse((publisher != "WashingtonPost") & grepl("-",b),b,
                 ifelse((publisher != "WashingtonPost") & grepl("-",a),a,
                 ifelse((publisher == "WashingtonPost") & grepl("-",g) & !grepl(".html",g),g,
                 ifelse((publisher == "WashingtonPost") & grepl("-",f) & !grepl(".html",f),f,
                 ifelse((publisher == "WashingtonPost") & grepl("-",e) & !grepl(".html",e),e,
                 ifelse((publisher == "WashingtonPost") & grepl("-",d) & !grepl(".html",d),d,
                 ifelse((publisher == "WashingtonPost") & grepl("-",c) & !grepl(".html",c),c,b
                 ))))))))))))))))

title$title <- gsub("-", " ", title$title, fixed=TRUE)
title$title <- gsub("_", " ", title$title, fixed=TRUE)
title$title <- gsub(".html", "", title$title, fixed=TRUE)
title$title <- gsub("u s", "us", title$title, fixed=TRUE)
```


### Getting words and bigrams
Words
```r
title2 <- title %>%
  select(GLOBALEVENTID,SQLDATE, publisher,title) %>%
  separate(title,into = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10",
                          "X11","X12","X13","X14","X15","X16","X17","X18","X19","X20"), 
           sep = " ", remove = FALSE, extra = "merge", fill = "right")

title3 <- title2 %>%
  select(-title) %>%
  melt(id=c('GLOBALEVENTID','SQLDATE','publisher'), na.rm = TRUE)
 
title3 <- title3 %>%
  filter(!(value %in% stopwords("en"))) %>%
  filter(value != "s") %>%
  mutate(SQLDATE = as.Date(as.character(SQLDATE), format = "%Y%m%d")) %>%
  mutate(Year = format(SQLDATE, "%Y")) %>%
  select(-variable)
```

Bigram
```r
title4 <- title %>%
  select(GLOBALEVENTID,SQLDATE, publisher,title) %>%
  mutate(SQLDATE = as.Date(as.character(SQLDATE), format = "%Y%m%d")) %>%
  mutate(Year = format(SQLDATE, "%Y")) %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

title4 <- title4 %>%
  separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!(word1 %in% stopwords("en"))) %>%
  filter(!(word2 %in% stopwords("en"))) %>%
  select(-word1,-word2) %>%
  filter(!is.na(bigram))

colnames(title4)[which(names(title4) == "bigram")] <- "value" 

title5 <- rbind(title4,title3) #combine the words and bigram

#head(title4,20)
```
```r
mention <- title5 %>%
  filter(value != "") %>%
  group_by(publisher, Year, value) %>%
  summarise(freq = n()) %>%
  ungroup()

title6 <- title3 %>%
  mutate(trump = ifelse(grepl("trump",title),1,0),
         obama = ifelse(grepl("obama",title),1,0))

events <- title %>%
  select(GLOBALEVENTID,trump,obama) %>%
  right_join(events,by = "GLOBALEVENTID")

#write.csv(mention,"mention.csv")
#write.csv(events,"events.csv")
```
**T-test of G-score**
```{r}
trump <- events %>%
  select(GoldsteinScale,trump) %>%
  filter(trump == 1)

obama <- events %>%
  select(GoldsteinScale,obama) %>%
  filter(obama == 1)

t.test(trump$GoldsteinScale, obama$GoldsteinScale)
```

Welch Two Sample t-test

data:  trump$GoldsteinScale and obama$GoldsteinScale<br />
t = -9.6799, df = 167930, p-value < 2.2e-16<br />
alternative hypothesis: true difference in means is not equal to 0<br />
95 percent confidence interval:<br />
 -0.1962014 -0.1301270<br />
sample estimates:<br />
mean of x mean of y<br />
0.7219720 0.8851362<br />

## Data Visualization(R)
Wordcloud of one-word and bigram
```r
mention%>%
  filter(Year == "2017") %>% #get yearly wordcloud
  select(-Year) %>%
  arrange(desc(freq)) %>%
  top_n(200) %>%
  wordcloud2(color = "random-light")                     
```
Pubisher(Washington Post, WSJ, New York Times, Bloomberg, Vogue, Cosmopolitan, New Yorker)
```r
mention %>%
  filter(publisher == "WSJ" & Year == "2017") %>% #get wordcloud by publisher and filter out by year
  select(-Year) %>%
  arrange(desc(freq)) %>%
  top_n(200) %>%
  wordcloud2(color = "random-light")
  ```
