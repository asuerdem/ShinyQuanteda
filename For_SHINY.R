#####WORKFLOW FOR QUANTEDA GUI #########
#0. BUILDING A CORPUS FOR TEXT ANALYSIS
############CSV APP##########

library(quanteda)
library(readtext)
library(quantedaData)
library(SnowballC)
library(ggplot2)
library(dplyr)
library(readr)
setwd("H:/text_ana_quanteda")
#0.0 find a csv  upload template from SHINY
#0.1 read the text into quanteda, availabale formats are:
#(.txt) ; (.csv) ;XML ; JSON; data Facebook API, in JSON ;data from  Twitter API,  
#from a drop down menu, user can select the suitable format
#READ COMMAND: text_field= is where the text is: ie. title, contenet, news etc... 
#hence we need another configuration button after the formatof the corpus is selected: 
#your text is in column: ....
aidata <- readtext("AI_peopleschina.csv", text_field="content") 
# following command turns the file into a corpus: 
aicorp <- corpus(aidata) #makes a corpus
#following command creates Metadata about the corpus, ie. description etc..
#a menu item such as: Description and Notes about your corpus: 
metacorpus(aicorp, "notes") <- "This is the description of my corpus........"
#1. EXPLORE YOUR CORPUS
#1.0 this gives a general summary of the corpus and makes a DF for further operations
#1.0.1: calculate readibility: a pulldown menu with configuration: measure= "Flesch.Kincaid" etc.. ; 
#remove_hyphens=T, F; min-max sentence length
fk <- textstat_readability(aicorp, "Flesch.Kincaid")
#add readibility to corpus
docvars(aicorp, "fk") <- fk
# 1.0.2 inspect the corpus summary
tokenInfo <-summary(aicorp, showmeta = T)
#write to an external csv file if needed, make a button: do you want save the summary as external file?
write.csv(tokenInfo, file="xyz")
#get longest ten: a pulldown menu Tokens, Types or Sentences or fk
head(arrange(tokenInfo,  desc(Tokens)), n=10)
#get shortest 10 a pulldown menu Tokens, Types or Sentences
head(arrange(tokenInfo, Tokens), n=10)
#1.1 different graphs for exploring the corpus:
#configuartion: x, y, plottype, some plots may require tweaking, as.factor, as.,nteger etc...
ggplot(tokenInfo, aes(x =as.factor(year), y=Tokens)) + geom_boxplot()
#configuartion: x, y, plottype, some plots may require tweaking, as.factor, as.,nteger etc...
ggplot(tokenInfo, aes(x =as.factor(year), y=fk)) + geom_boxplot()
#  to see  the content of any text, beeter done with plotly in an intercative manner, if noT: 
texts(aicorp)[76]
#1.2 corpus_subset
ai16 <- corpus_subset(aicorp, year == 2016)
ai15 <- corpus_subset(aicorp, year == 2015)
#join two corpuses, ie update your corpus, append new elements
aicorp <- aicorp + "newcorp"
aisum <- ai15 + ai16 #this is a working example, 

#1.3 concordance: box: enter your keyword, windows=?
options(width = 200)
scikw <- kwic(aicorp, "science")
kwic(aicorp, "scien", valuetype = "regex")
textplot_xray(scikw, sort=T)
#lexical dispersion plot
textplot_xray(kwic(ai15, "science" ), kwic(ai15, "technology" ), sort = T)+ 
  aes(color = keyword) + scale_color_manual(values = c("blue", "red"))
#subset the kw corpus
KWsubset <- corpus_subset(aicorp, docnames(aicorp)%in% scikw$docname)#subsets the documents of which names match the kwic docs(home)
write.csv(KWsubset, file = "xxx")


#2. FEATURE EXTRACTION
#2.0 tOKENIZE
#TOKENIZE: configuration: what: "word", "sentence", "character", "fastestword; 
#remove: symbols, separators, hyphens, twitter, urls, numbers, puntuation
#n-grams? 1L, 2L ...
aitokens <- tokens(aicorp, remove_punct=T, remove_separators = T, remove_numbers = T, remove_url = T)
#remove stopwords configuration: language (english, turkish.... ); case_insensitive, min_nchar, max_nchar
aitokenstp <- tokens_select(aitokens, stopwords("SMART"), "remove", padding = F)
#n-grams
aingr <- tokens_select(tokens(aicorp, remove_punct = T, ngrams = 2), stopwords("english"), "remove")
#to remove the stopwords for n-grams:#COLLOCATIONS: configuration: w or w/o stopwords?
colls <- textstat_collocations(aitokenstp, n = 1500, size = 2)
arrange(colls, desc(count))
#####need to install rspacy, python
colls <- textstat_collocations(aitokenstp, n = 1500, size = 2)

#Constructing a document-frequency matrix; this is a quick and dirty solution, needs to be done after careful feature selection
ai.dfm <- dfm(aicorp, remove = stopwords("english"), stem = T, remove_punct = T, remove_numbers = T)
topfeatures(ai.dfm, 100)  #  top words
#trim
ai.trm <- dfm_trim(ai.dfm, min_count = 250, verbose = T)

set.seed(100)
textplot_wordcloud(ai.trm, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))


#TF (term frequency), term frequency/allterms in a doc;
aidfm.tf <- tf(ai.dfm, scheme="prop")
topfeatures(aidfm.tf, 100)  #  top words
textplot_wordcloud(aidfm.tf, min.freq = .5, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

# TF* IDF (Inverse document frequency), log (occurence of term in doc/all docs)
ai.tfidf <- tfidf(ai.dfm)
topfeatures(ai.tfidf, 100)  #  top words
#wordcloud
textplot_wordcloud(ai.tfidf, min.freq = 200, random.order = F,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

#Frequency table
aifr <- textstat_frequency(ai.tfidf, n = 100)
# Sort by reverse frequency order
aifr$feature <- with(aifr, reorder(feature, -frequency))
ggplot(aifr, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Grouping
ai.grp <- dfm(aicorp, groups = "year", remove = stopwords("SMART"), remove_punct = TRUE)
dfm_sort(ai.grp)[, 1:20]
textplot_wordcloud(ai.grp, comparison = T)
#Baloonplot for 
aigr.trm <- dfm_trim(ai.grp, min_count = 500, verbose = T)
dt <- as.table(as.matrix(aigr.trm))
library("gplots")
balloonplot(t(dt), main ="Words", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Get frequency grouped by 
dfm_weight_year <- aicorp %>%
  dfm(remove = stopwords("english"), remove_punct = TRUE) %>%
  dfm_weight("relfreq")
topfeatures((dfm_weight_year))
freq_grouped <- textstat_frequency(dfm_weight_year,
                                   groups = "year")
# Filter the term "home"
freq_home<- subset(freq_grouped, feature %in% "musk") 
ggplot(freq_home,  aes(group, frequency)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#do for most frequent each year
freq_grouped <- textstat_frequency(dfm_weight_year, n=10,
                                   groups = "year")

ggplot(data = freq_grouped, aes(x = nrow(freq_grouped):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_grouped):1,
                     labels = freq_grouped$feature) +
  labs(x = NULL, y = "Relative frequency")


#DFM from dictionary categories
myDict <- dictionary(list(science = c("technology", "scien*", "invent*"),
                          economy = c("jobs", "economy", "business", "grow", "work")))
ai.dict <- dfm(aicorp, groups = "year",  remove = stopwords("english"), remove_punct = TRUE, dictionary =myDict)
topfeatures(ai.dict)

#Baloonplot for 
dt <- as.table(as.matrix(ai.dict))
library("gplots")
balloonplot(t(dt), main ="Words", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# Only select speeches by Obama and Trump
pres_corpus <- corpus_subset(aicorp, 
                             year %in% c("2015", "2016"))

# Create a dfm grouped by president
pres_dfm <- dfm(pres_corpus, groups = "year", remove = stopwords("english"), 
                remove_punct = TRUE)
# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(pres_dfm, target = "2015")
# Plot estimated word keyness
textplot_keyness(result_keyness) 

#
if (require(topicmodels)) {
  myLDAfit20 <- LDA(convert(ai.trm, to = "topicmodels"), k = 20)
  get_terms(myLDAfit20, 5)
}
