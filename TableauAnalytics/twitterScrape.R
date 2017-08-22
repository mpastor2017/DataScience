## Twitter Analysis
library(tm)
library(twitteR)
library(data.table)

tokens = character()

fileName <- "./SocialTextFiles/TwitterAuth.txt"
conn <- file(fileName,open="r")
linn <-readLines(conn)
close(conn)

consumer_key <- strsplit(linn[1],':')[[1]][2]
consumer_secret <- strsplit(linn[2],':')[[1]][2]
access_token <- strsplit(linn[3],':')[[1]][2]
access_secret <- strsplit(linn[4],':')[[1]][2]

## 
enitity = "Candidate Twitter Name Here"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#### Grabbing latest tweets
feed = data.table(twListToDF(searchTwitter(entity, n = 3200)))

#### Cleaning function
cleanup <- function(x) {
        x <- sapply(x,function(row) iconv(row, "latin1", "ASCII", sub=""))
        x <- tolower(x)
        x <- gsub("<img src.*?>", "", x)
        x <- gsub("http\\S+", "", x)
        x <- gsub("\\[math\\]", "", x) # text between [] refers to tags e.g. [math]
        x <- gsub("<.*?>", " ", x)
        x <- gsub("\n", " ", x)  # replace newline with a space
        x <- gsub("\\s+", " ", x)  # multiple spaces into one
        # using tm_map to remove stopwords
        docs <- Corpus(VectorSource(x))
        docs <- tm_map(docs, removeWords, stopwords('en'))
        docs <- tm_map(docs, removePunctuation)    # dont remove punct so early in the analysis
        docs <- tm_map(docs, stripWhitespace)
        xxx <- sapply(docs, function(i) i)
        data_content <- data.frame(text = xxx, stringsAsFactors = FALSE)
        data_content$text <- gsub("\\d+", "", data_content$text)
        return(data_content$text)
}

feed$original_text = feed$text

## May choose to clean script here, in python, or use both
feed$text = cleanup(feed$text)

### Saving as CSVs for Python Data Crunching
fwrite(feed, paste("./UnprocessedCSV/",entity,"_twitter.csv", sep = ''))

