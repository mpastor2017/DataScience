library(tm)
library(data.table)

## Creating Corpus
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
corpusBuilder = function(x){
        RemoveAMP <- function(y){
                gsub("amp","",y)
        }
        corpus = Corpus(VectorSource(x))
        corpus = tm_map(corpus, content_transformer(RemoveAMP))
        return(corpus)
}

fb = fread('./ProcessedCSV/Facebook.csv')
twit = fread('./ProcessedCSV/Twitter.csv')

## Cleanup text
fb$comment_message = cleanup(fb$comment_message)
twit$text = cleanup(twit$text)


word_func = function(candidate){
        f = subset(fb, fb$candidate == as.character(candidate))
        t = subset(twit, twit$candidate == as.character(candidate))
        
        colnames(f)[4] = 'text'
        
        # Facebook
        f = paste(unlist(f$text), collapse =" ")
        freq = table(strsplit(f, " "))
        f = data.table(cbind(names(freq),as.integer(freq)))
        names(f) = c("Word", "Count") 
        f$Social_Media = 'Facebook'
        f = f[-1,]
        
        #Twitter
        t = paste(unlist(t$text), collapse =" ")
        freq = table(strsplit(t, " "))
        t = data.table(cbind(names(freq),as.integer(freq)))
        names(t) = c("Word", "Count") 
        t$Social_Media = 'Twitter'
        t = t[-1,]
        
        word_count = rbind(f,t)
        
        fwrite(word_count, paste('./ProcessedCSV/',candidate,'_words.csv', sep = ''))
        return(word_count)
        
}


