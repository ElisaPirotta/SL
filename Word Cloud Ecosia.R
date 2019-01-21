## ----
# Create a Word Cloud
## ----

# Scraping Ecosia - Instagram

library(tm)
library(stringr)
library(readxl)

EcosiaecosiaInstagra <- read_excel("C:/Users/stefano/Downloads/ThisCrush/EcosiaecosiaInstagra.xlsx")
EcosiaecosiaInstagra <- as.data.frame(EcosiaecosiaInstagra)
df<-EcosiaecosiaInstagra

# ----
# standardizziamo il testo

df$Content <- sapply(df$Content,function(row) iconv(row, "latin1", "ASCII", sub="")) # toglie caratteri speciali
myCorpus <- Corpus(VectorSource(df$Content)) # crea oggetto Text Mining
myCorpus <- tm_map(myCorpus, content_transformer(tolower)) # riduce tutto in minuscolo
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) # elimina gli url
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) # elimina la punteggiatura
myCorpus <- tm_map(myCorpus, removeNumbers) # elimina i numeri
myStopwords <- c(stopwords(kind="en"), stopwords(kind = "it"), "can", "will", "also") # parole non significative italiane e inglesi
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) # elimina le stopwords

# ----
# funzioni per contare le parole

tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
tdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))
findFreqTerms(tdm, lowfreq=40)

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=20)
termFrequency[order(-termFrequency)]

# ----
# funzioni per rappresentare le parole in un grafico

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=40)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity", fill = "mediumspringgreen", colour = "peachpuff") + xlab("Terms") + ylab("Count") + coord_flip() 

# ----
# funzione per vedere le parole associate ad una parola chiave 
findAssocs(tdm, 'great', 0.20)

# ----
# funzione per realizzare il wordcloud
library(wordcloud2)
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)

fr <- as.data.frame(wordFreq)
fr <- cbind(fr, names(wordFreq))
fr[,2] <- fr[,1]
fr[,1] <- names(wordFreq)
colnames(fr)[1] <- "Word"
colnames(fr)[2] <-"Freq"
fr$Word <- as.factor(fr$Word)

wordcloud2(fr, size = 1, color = "random-light", backgroundColor = "bold", shuffle = T)

## ----
#
## ----

library(tm)
library(stringr)
library(readr)

# Scraping Ecosia - Twitter


df_ecosia <- read_delim("C:/Users/stefano/Desktop/Magistrale2018-2019/SL/SL_Project Ecosia/df_ecosia.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
df_ecosia <- as.data.frame(df_ecosia)
df<-df_ecosia

# ----
# standardizziamo il testo

df$contenuto <- sapply(df$contenuto,function(row) iconv(row, "latin1", "ASCII", sub="")) # toglie caratteri speciali
myCorpus <- Corpus(VectorSource(df$contenuto)) # crea oggetto Text Mining
myCorpus <- tm_map(myCorpus, content_transformer(tolower)) # riduce tutto in minuscolo
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) # elimina gli url
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) # elimina la punteggiatura
myCorpus <- tm_map(myCorpus, removeNumbers) # elimina i numeri
myStopwords <- c(stopwords(kind="en"), stopwords(kind = "it"), "can", "will", "also", "just") # parole non significative italiane e inglesi
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) # elimina le stopwords

# ----
# funzioni per contare le parole

tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
tdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))
findFreqTerms(tdm, lowfreq=10)

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
termFrequency[order(-termFrequency)]

# ----
# funzioni per rappresentare le parole in un grafico

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity", fill = "turquoise1") + xlab("Terms") + ylab("Count") + coord_flip() + theme_minimal()

# ----
# funzione per vedere le parole associate ad una parola chiave 
findAssocs(tdm, 'engine', 0.20)


# ----
# funzione per realizzare il wordcloud
library(wordcloud2)
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)

fr <- as.data.frame(wordFreq)
fr <- cbind(fr, names(wordFreq))
fr[,2] <- fr[,1]
fr[,1] <- names(wordFreq)
colnames(fr)[1] <- "Word"
colnames(fr)[2] <-"Freq"
fr$Word <- as.factor(fr$Word)

wordcloud2(fr, size = 1, color = "random-light", backgroundColor = "bold", shuffle = T)
