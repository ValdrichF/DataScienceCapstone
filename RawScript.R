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
    geom_line(aes(1:nrow(a), CumFreq), size = 1, color = 'steelblue')+
    geom_line(aes(1:nrow(a), Freq), size= 1, color = 'red')
which.max(a$CumFreq>0.5)
which.max(a$CumFreq>0.9)
# Function to form unique pairs (pattern)


# Calculate the number of times the pairs appear
str_count(paste0(twitterWords, collapse = ' '), pattern = "yabadabadooo")
# Plot it

#
