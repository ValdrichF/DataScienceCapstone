library(dplyr)
library(stringr)
library(ggplot2)
# Downloading the data
if(!dir.exists('./Data')){
    dir.create('Data')
    download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
                  'Coursera-SwiftKey.zip')
    unzip('Coursera-SwiftKey.zip', exdir = 'Data', junkpaths = TRUE,
          files = c('final/en_US/en_US.twitter.txt',
                    'final/en_US/en_US.news.txt',
                    'final/en_US/en_US.blogs.txt'))
}

# Importing a 'random' sample of the datasets of size 4000
set.seed(123)
twitter = readLines('./Data/en_US.twitter.txt')%>%
    sample(4000)
news    = readLines('./Data/en_US.news.txt')%>%
    sample(4000)
blogs   = readLines('./Data/en_US.blogs.txt')%>%
    sample(4000)

# Function to split the strings into words
words = function(.line){
    # x = str_replace_all(x, '\\W|\\d', ' ')
    x = tolower(.line)%>%
        str_split(pattern ='\\s')%>%
        unlist%>%
        na_if('')%>%
        na_if(' ')
    x[!is.na(x)]
}

# Funtion to remove profanity words
profan = function(words){
    p = readLines('./Data/EnProfanity.txt')
    words[!(words %in%p)]
}

# Function to remove punctions
punc = function(words){
    words = sapply(words, str_split, pattern = "[:punct:]")%>%
        unlist%>%
        na_if('')%>%
        na_if(' ')
    words = words[-grep("[[:punct:]]", words)]
    words[!is.na(words)]
}

# Testing on twitter dataset
twitterWords = words(twitter)%>%
    profan%>%
    punc
names(twitterWords) = NULL

# Which words are the most frequent?
a = as.data.frame(table(twitterWords))
a = a[order(a$Freq, decreasing = T),]%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))

# How many unique words do you need in a frequency sorted dictionary to cover 50%
# of all word instances in the language? 90%?
ggplot(a)+
    geom_line(aes(1:nrow(a), CumFreq*100), size = 1, color = 'steelblue')+
    labs(x = 'Rank of word by frequency',
         y = 'Cumulitive Frequency',
         title = 'Percent words used in the text')
which.max(a$CumFreq>0.5)
which.max(a$CumFreq>0.9)

# Function to form pairs of words
bigram = function(words){
    if (length(words) >= 2){
        paste(words[-length(words)],words[-1], sep = "_")
    }else warning("The arguement 'words' must have length > 1")
}

twitterWordPairs = bigram(twitterWords)
freq = as.data.frame(table(twitterWordPairs))
freq = freq[order(freq$Freq, decreasing = T),]%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))

# Plot it
ggplot(freq)+
    geom_line(aes(1:nrow(freq)/nrow(freq), CumFreq), size = 1, color = 'steelblue')+
    labs(x = 'Rank of word pairs by frequency',
         y = 'Cumulitive Frequency',
         title = 'Percent word pairs used in the text')
which.max(freq$CumFreq>0.5)
which.max(freq$CumFreq>0.9)

#
