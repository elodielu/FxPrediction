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
  separate(path,into = c("a","b","c","d","e","f","g","h","i","j"), sep = "/", remove = FALSE, extra = "merge", fill = "right") %>%
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
                        
                     
