## How to make the data speak
### Data Extraction from BigQuery(SQL)

### Data Wrangeling(R)
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
#### Text Processing
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
```

#### Getting words and bigrams
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

Wordcloud of one-word and bigram
```r
title5 %>%
  group_by(Year, value) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  filter(Year == "2014") %>%
  select(-Year) %>%
  arrange(desc(freq)) %>%
  top_n(200) %>%
  wordcloud2(color = "random-light")                     
```
