# Install and load the required packages.
pacman::p_load(dplyr, ggplot2, stringr, udpipe, lattice)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(tm)
library(SnowballC)
library(tidytext)
library(lubridate)
library(scales)

#reading the file
google2004 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT1.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2004)
cleanedDoc <- gsub("TABLE_END", "", google2004)
cleanedDoc <- gsub("TABLESTART", "", google2004)
cleanedDoc <- gsub("sites", "", google2004)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# Step 5: extract and display frequencies for universal parts of speech (upos) in text
stats<-txt_freq(x$upos)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=stats,col="yellow",main="UPOS(Universal Parts of Speech)\n
         frequency of occurence",xlab="Freq")
# Step 6: extract and display most occurring nouns in the headlines

stats<-subset(x,upos %in% c("NOUN"))
stats<-txt_freq(stats$token)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=head(stats,20),col="cadetblue",main="Most occurring nouns", xlab="Freq")

# Step 7: extract and display most occuring adjectives in the headlines

stats<-subset(x,upos %in% c("ADJ"))
stats<-txt_freq(stats$token)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=head(stats,20),col="purple",main="Most occurring adjectives",xlab="Freq")

# Step 8 : extract and display most occuring verbs in the headlines

stats<-subset(x,upos %in% c("VERB"))
stats<-txt_freq(stats$token)
stats$key<-factor(stats$key,levels=rev(stats$key))
barchart(key~freq,data=head(stats,20),col="gold",main="Most occurring Verbs",xlab="Freq")

# Step 9: finally use RAKE (Rapid Automatic Keyword Extraction algorithm) to 
# determine key phrases in a body of text by analyzing the frequency of word appearance
# and its co-occurence with other words in the text.

stats<-keywords_rake(x=x,term="lemma",group="doc_id",relevant = x$upos %in% c("NOUN","ADJ"))
stats$key<-factor(stats$keyword,levels=rev(stats$keyword))
barchart(key~freq,data=head(subset(stats,freq>3),20),col="red",main="2007 - Keywords identified by RAKE",xlab="Rake")

# Step 10 : In English  This may be useful for understanding context of a sentence or a review 
# or headlines especially if they are clickbait like. This step is to just extract top phrases
#that are basically keyword topics 

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")


# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2004 - Keywords identified by RAKE",
         xlab = "Rake")

#2005
#reading the file
google2005 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2005)
cleanedDoc <- gsub("TABLE_END", "", google2005)
cleanedDoc <- gsub("TABLESTART", "", google2005)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2005)
cleanedDoc <- gsub("sites", "", google2005)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2005 - Keywords identified by RAKE",
         xlab = "Rake")

#2006
#reading the file
google2006 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT3.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2006)
cleanedDoc <- gsub("TABLE_END", "", google2006)
cleanedDoc <- gsub("TABLESTART", "", google2006)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2006)
cleanedDoc <- gsub("sites", "", google2006)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2006 - Keywords identified by RAKE",
         xlab = "Rake")

#2007
#reading the file
google2007 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT4.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2007)
cleanedDoc <- gsub("TABLE_END", "", google2007)
cleanedDoc <- gsub("TABLESTART", "", google2007)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2007)
cleanedDoc <- gsub("sites", "", google2007)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2007 - Keywords identified by RAKE",
         xlab = "Rake")

#2008
#reading the file
google2008 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT5.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2008)
cleanedDoc <- gsub("TABLE_END", "", google2008)
cleanedDoc <- gsub("TABLESTART", "", google2008)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2008)
cleanedDoc <- gsub("sites", "", google2008)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2008 - Keywords identified by RAKE",
         xlab = "Rake")
#2009
#reading the file
google2009 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT6.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2009)
cleanedDoc <- gsub("TABLE_END", "", google2009)
cleanedDoc <- gsub("TABLESTART", "", google2009)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2009)
cleanedDoc <- gsub("sites", "", google2009)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2009 - Keywords identified by RAKE",
         xlab = "Rake")

#2010
#reading the file
google2010 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT7.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2010)
cleanedDoc <- gsub("TABLE_END", "", google2010)
cleanedDoc <- gsub("TABLESTART", "", google2010)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2010)
cleanedDoc <- gsub("sites", "", google2010)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2010 - Keywords identified by RAKE",
         xlab = "Rake")

#2011
#reading the file
google2011 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT8.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2011)
cleanedDoc <- gsub("TABLE_END", "", google2011)
cleanedDoc <- gsub("TABLESTART", "", google2011)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2011)
cleanedDoc <- gsub("sites", "", google2011)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2011 - Keywords identified by RAKE",
         xlab = "Rake")

#2012
#reading the file
google2012 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT9.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2012)
cleanedDoc <- gsub("TABLE_END", "", google2012)
cleanedDoc <- gsub("TABLESTART", "", google2012)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2012)
cleanedDoc <- gsub("sites", "", google2012)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2012 - Keywords identified by RAKE",
         xlab = "Rake")
#2013
#reading the file
google2013 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT10.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2013)
cleanedDoc <- gsub("TABLE_END", "", google2013)
cleanedDoc <- gsub("TABLESTART", "", google2013)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2013)
cleanedDoc <- gsub("sites", "", google2013)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2013 - Keywords identified by RAKE",
         xlab = "Rake")

#2014
#reading the file
google2014 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT11.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2014)
cleanedDoc <- gsub("TABLE_END", "", google2014)
cleanedDoc <- gsub("TABLESTART", "", google2014)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2014)
cleanedDoc <- gsub("sites", "", google2014)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2014 - Keywords identified by RAKE",
         xlab = "Rake")

#2015
#reading the file
google2015 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT12.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2015)
cleanedDoc <- gsub("TABLE_END", "", google2015)
cleanedDoc <- gsub("TABLESTART", "", google2015)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2015)
cleanedDoc <- gsub("sites", "", google2015)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2015 - Keywords identified by RAKE",
         xlab = "Rake")

#2016
#reading the file
google2016 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT13.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2016)
cleanedDoc <- gsub("TABLE_END", "", google2016)
cleanedDoc <- gsub("TABLESTART", "", google2016)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2016)
cleanedDoc <- gsub("sites", "", google2016)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2016 - Keywords identified by RAKE",
         xlab = "Rake")


#2017
#reading the file
google2017 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT14.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2017)
cleanedDoc <- gsub("TABLE_END", "", google2017)
cleanedDoc <- gsub("TABLESTART", "", google2017)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2017)
cleanedDoc <- gsub("sites", "", google2017)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2017 - Keywords identified by RAKE",
         xlab = "Rake")

#2018
#reading the file
google2018 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT15.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2018)
cleanedDoc <- gsub("TABLE_END", "", google2018)
cleanedDoc <- gsub("TABLESTART", "", google2018)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2018)
cleanedDoc <- gsub("sites", "", google2018)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2018 - Keywords identified by RAKE",
         xlab = "Rake")

#2019
#reading the file
google2019 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT16.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2019)
cleanedDoc <- gsub("TABLE_END", "", google2019)
cleanedDoc <- gsub("TABLESTART", "", google2019)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2019)
cleanedDoc <- gsub("sites", "", google2019)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2019 - Keywords identified by RAKE",
         xlab = "Rake")

#2020
#reading the file
google2020 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT17.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2020)
cleanedDoc <- gsub("TABLE_END", "", google2020)
cleanedDoc <- gsub("TABLESTART", "", google2020)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2020)
cleanedDoc <- gsub("sites", "", google2020)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2020 - Keywords identified by RAKE",
         xlab = "Rake")

#2021
#reading the file
google2021 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT18.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2021)
cleanedDoc <- gsub("TABLE_END", "", google2021)
cleanedDoc <- gsub("TABLESTART", "", google2021)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("INFORMATION", "", google2021)
cleanedDoc <- gsub("sites", "", google2021)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "2021 - Keywords identified by RAKE",
         xlab = "Rake")


#2004-2010
#reading the file
google2004_2010 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2004_2010.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2004_2010)
cleanedDoc <- gsub("TABLE_END", "", google2004_2010)
cleanedDoc <- gsub("TABLESTART", "", google2004_2010)
cleanedDoc <- gsub("INFORMATION", "", google2004_2010)
cleanedDoc <- gsub("sites", "", google2004_2010)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "Years 2004-2010 - Keywords identified by RAKE",
         xlab = "Rake")


#2011-2015
#reading the file
google2011_2015 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2011_2015.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2011_2015)
cleanedDoc <- gsub("TABLE_END", "", google2011_2015)
cleanedDoc <- gsub("TABLESTART", "", google2011_2015)
cleanedDoc <- gsub("INFORMATION", "", google2011_2015)
cleanedDoc <- gsub("sites", "", google2011_2015)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "skyblue",
         main = "Years 2011-2015 - Keywords identified by RAKE",
         xlab = "Rake")


#2016-2021
#reading the file
google2016_2021 <- readxl::read_excel("/Users/sanjeevh/Downloads/ACCT2016_2021.xlsx")

cleanedDoc <- gsub("<.*?>", "", google2016_2021)
cleanedDoc <- gsub("TABLE_END", "", google2016_2021)
cleanedDoc <- gsub("TABLESTART", "", google2016_2021)
cleanedDoc <- gsub("INFORMATION", "", google2016_2021)
cleanedDoc <- gsub("sites", "", google2016_2021)
cleanedDoc <- gsub("additional", "", cleanedDoc)
cleanedDoc <- gsub("&nbsp;"," ", cleanedDoc)
cleanedDoc <- gsub(" {2,}", "", cleanedDoc)
cleanedDoc <- gsub("^\\s+|\\s+$", "", cleanedDoc)
cleanedDoc <- gsub("\\d+", "", cleanedDoc)
cleanedDoc <- gsub("&#;", "", cleanedDoc)
cleanedDoc <- gsub("[^[:alnum:][:blank:]+?&/\\-]", "", cleanedDoc)
cleanedDoc <- cleanedDoc[cleanedDoc != ""]
cleanedDoc <- cleanedDoc[cleanedDoc != " "]
cleanedDoc <- cleanedDoc[cleanedDoc != ",,"]
cleanedDoc <- cleanedDoc[cleanedDoc != ","]

udmodel_english <- udpipe_load_model(file = "/Users/sanjeevh/Downloads/english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, cleanedDoc)
x <- data.frame(s)

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "orange",
         main = "Years 2016-2021 - Keywords identified by RAKE",
         xlab = "Rake")
