# Add the required libraries and the data unless you have it already loaded
# Install and load the required packages.
pacman::p_load(tidyr, dplyr, stringr, data.table, sentimentr, ggplot2)
pacman::p_load(tidyr, tidytext, tidyverse, dplyr, stringr, ggplot2, textdata, yarrr, radarchart,
               wordcloud2 )

#2004

# load the data unless you already have it loaded
google2004 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT1.xlsx")

#DATA CLEANING
google2004<- google2004['TEXT']

#drop na
google2004<- na.omit(google2004)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2004 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2004_words_filtered <- google2004 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2004_nrc <- google2004_words_filtered %>% inner_join(get_sentiments("nrc"))

google2004_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2004")

#2005

# load the data unless you already have it loaded
google2005 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2.xlsx")

#DATA CLEANING
google2005<- google2005['TEXT']

#drop na
google2005<- na.omit(google2005)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2005 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2005_words_filtered <- google2005 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2005_nrc <- google2005_words_filtered %>% inner_join(get_sentiments("nrc"))

google2005_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2005")


#2006
# load the data unless you already have it loaded
google2006 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT3.xlsx")

#DATA CLEANING
google2006<- google2006['TEXT']

#drop na
google2006<- na.omit(google2006)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2006 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2006_words_filtered <- google2006 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2006_nrc <- google2006_words_filtered %>% inner_join(get_sentiments("nrc"))

google2006_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2006")


#2007
# load the data unless you already have it loaded
google2007 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT4.xlsx")

#DATA CLEANING
google2007<- google2007['TEXT']

#drop na
google2007<- na.omit(google2007)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2007 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2007_words_filtered <- google2007 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2007_nrc <- google2007_words_filtered %>% inner_join(get_sentiments("nrc"))

google2007_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2007")


#2008
# load the data unless you already have it loaded
google2008 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT5.xlsx")

#DATA CLEANING
google2008<- google2008['TEXT']

#drop na
google2008<- na.omit(google2008)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2008 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2008_words_filtered <- google2008 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2008_nrc <- google2008_words_filtered %>% inner_join(get_sentiments("nrc"))

google2008_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2008")


#2009
# load the data unless you already have it loaded
google2009 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT6.xlsx")

#DATA CLEANING
google2009<- google2009['TEXT']

#drop na
google2009<- na.omit(google2009)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2009 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2009_words_filtered <- google2009 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2009_nrc <- google2009_words_filtered %>% inner_join(get_sentiments("nrc"))

google2009_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2009")


#2010
# load the data unless you already have it loaded
google2010 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT7.xlsx")

#DATA CLEANING
google2010<- google2010['TEXT']

#drop na
google2010<- na.omit(google2010)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2010 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2010_words_filtered <- google2010 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2010_nrc <- google2010_words_filtered %>% inner_join(get_sentiments("nrc"))

google2010_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2010")


#2011
# load the data unless you already have it loaded
google2011 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT8.xlsx")

#DATA CLEANING
google2011<- google2011['TEXT']

#drop na
google2011<- na.omit(google2011)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2011 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2011_words_filtered <- google2011 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2011_nrc <- google2011_words_filtered %>% inner_join(get_sentiments("nrc"))

google2011_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2011")

#2012
# load the data unless you already have it loaded
google2012 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT9.xlsx")

#DATA CLEANING
google2012<- google2012['TEXT']

#drop na
google2012<- na.omit(google2012)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2012 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2012_words_filtered <- google2012 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2012_nrc <- google2012_words_filtered %>% inner_join(get_sentiments("nrc"))

google2012_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2012")


#2013
# load the data unless you already have it loaded
google2013 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT10.xlsx")

#DATA CLEANING
google2013<- google2013['TEXT']

#drop na
google2013<- na.omit(google2013)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2013 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2013_words_filtered <- google2013 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2013_nrc <- google2013_words_filtered %>% inner_join(get_sentiments("nrc"))

google2013_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2013")


#2014
# load the data unless you already have it loaded
google2014 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT11.xlsx")

#DATA CLEANING
google2014<- google2014['TEXT']

#drop na
google2014<- na.omit(google2014)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2014 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2014_words_filtered <- google2014 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2014_nrc <- google2014_words_filtered %>% inner_join(get_sentiments("nrc"))

google2014_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2014")

#2015
# load the data unless you already have it loaded
google2015 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT12.xlsx")

#DATA CLEANING
google2015<- google2015['TEXT']

#drop na
google2015<- na.omit(google2015)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2015 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2015_words_filtered <- google2015 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2015_nrc <- google2015_words_filtered %>% inner_join(get_sentiments("nrc"))

google2015_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2015")


#2016
# load the data unless you already have it loaded
google2016 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT13.xlsx")

#DATA CLEANING
google2016<- google2016['TEXT']

#drop na
google2016<- na.omit(google2016)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2016 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2016_words_filtered <- google2016 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2016_nrc <- google2016_words_filtered %>% inner_join(get_sentiments("nrc"))

google2016_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2016")


#2017
# load the data unless you already have it loaded
google2017 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT14.xlsx")

#DATA CLEANING
google2017<- google2017['TEXT']

#drop na
google2017<- na.omit(google2017)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2017 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2017_words_filtered <- google2017 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2017_nrc <- google2017_words_filtered %>% inner_join(get_sentiments("nrc"))

google2017_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2017")


#2018
# load the data unless you already have it loaded
google2018 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT15.xlsx")

#DATA CLEANING
google2018<- google2018['TEXT']

#drop na
google2018<- na.omit(google2018)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2018 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2018_words_filtered <- google2018 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2018_nrc <- google2018_words_filtered %>% inner_join(get_sentiments("nrc"))

google2018_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2018")



#2019
# load the data unless you already have it loaded
google2019 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT16.xlsx")

#DATA CLEANING
google2019<- google2019['TEXT']

#drop na
google2019<- na.omit(google2019)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2019 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2019_words_filtered <- google2019 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2019_nrc <- google2019_words_filtered %>% inner_join(get_sentiments("nrc"))

google2019_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2019")


#2020
# load the data unless you already have it loaded
google2020 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT17.xlsx")

#DATA CLEANING
google2020<- google2020['TEXT']

#drop na
google2020<- na.omit(google2020)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2020 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2020_words_filtered <- google2020 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2020_nrc <- google2020_words_filtered %>% inner_join(get_sentiments("nrc"))

google2020_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2020")



#2021
# load the data unless you already have it loaded
google2021 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT18.xlsx")

#DATA CLEANING
google2021<- google2021['TEXT']

#drop na
google2021<- na.omit(google2021)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2021 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2021_words_filtered <- google2021 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2021_nrc <- google2021_words_filtered %>% inner_join(get_sentiments("nrc"))

google2021_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2021")


#2021
# load the data unless you already have it loaded
google2021 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT18.xlsx")

#DATA CLEANING
google2021<- google2021['TEXT']

#drop na
google2021<- na.omit(google2021)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2021 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2021_words_filtered <- google2021 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2021_nrc <- google2021_words_filtered %>% inner_join(get_sentiments("nrc"))

google2021_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for 2021")


#2004-2010
# load the data unless you already have it loaded
google2004_2010 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2004_2010.xlsx")

#DATA CLEANING
google2004_2010<- google2004_2010['TEXT']

#drop na
google2004_20101<- na.omit(google2004_2010)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2004_2010 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2004_2010_words_filtered <- google2004_2010 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2004_2010_nrc <- google2004_2010_words_filtered %>% inner_join(get_sentiments("nrc"))

google2004_2010_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for Years 2004 - 2010")


#2011-2015
# load the data unless you already have it loaded
google2011_2015 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2011_2015.xlsx")

#DATA CLEANING
google2011_2015<- google2011_2015['TEXT']

#drop na
google2011_2015<- na.omit(google2011_2015)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2011_2015 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2011_2015_words_filtered <- google2011_2015 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2011_2015_nrc <- google2011_2015_words_filtered %>% inner_join(get_sentiments("nrc"))

google2011_2015_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for Years 2011 - 2015")


#2016-2021
# load the data unless you already have it loaded
google2016_2021 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2016_2021.xlsx")

#DATA CLEANING
google2016_2021<- google2016_2021['TEXT']

#drop na
google2016_2021<- na.omit(google2016_2021)

remove_words <- c("TABLESTART","TABLEEND","TABLE","START","END","google","sites","table_end","table_start")

#Delete these words 
#unnest and remove stop, undesirable and short words
google2016_2021 %>% select(TEXT) %>% unnest_tokens(word,TEXT) %>% 
  filter(!word %in% remove_words) %>%
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

google2016_2021_words_filtered <- google2016_2021 %>%
  unnest_tokens(word, TEXT) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% remove_words) %>%
  filter(nchar(word) > 3)

nrc <- get_sentiments("nrc") %>% mutate(lexicon = "nrc", words_in_lexicon = n_distinct(word))
View (nrc)

google2016_2021_nrc <- google2016_2021_words_filtered %>% inner_join(get_sentiments("nrc"))

google2016_2021_words_filtered %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  ggplot(aes(x = sentiment,fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = c('positive'='Green', 'negative'='Red', 'anger'='Dark red','anticipation'='Yellow',
                               'disgust'='Brown','fear'='Purple','joy'='Light green', 'sadness'='blue','surprise'='orange',
                               'trust'= 'Pink')) +
  ggtitle("Sentiments for Years 2016 - 2021")


