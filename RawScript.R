library(dplyr)
library(stringr)
library(ggplot2)
library(ngram)
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
twitter = readLines('./Data/en_US.twitter.txt')
subInt = as.logical(rbinom(twitter,1,prob = 0.001))
twitter = twitter[subInt]
news    = readLines('./Data/en_US.news.txt')
subInt = as.logical(rbinom(news,1,prob = 0.001))
news = news[subInt]
blogs   = readLines('./Data/en_US.blogs.txt')
subInt = as.logical(rbinom(blogs,1,prob = 0.001))
blogs = blogs[subInt]
remove(subInt)

# Function to remove punctions
punc = function(Lines){
    # Remove punctuations except contractions eg. can't
    str_replace_all(Lines, "(('t)|('ll)|('m)|('re)|('s)|('d)|('ve))|[[:punct:]]", "\\1")
}

# Remove numbers
numb = function(Lines){
    str_replace_all(Lines, '\\d', '')
}

# Function to split the strings into words
words = function(.line){
    x = tolower(.line)%>%
        str_split(pattern ='\\s')%>%
        unlist%>%
        na_if('')%>%
        na_if(' ')
    x[!is.na(x)]
}

# text file with English Profanity words from
# https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
profanity = readLines('./Data/EnProfanity.txt')
profanityWords = profanity[-grep(' ', profanity)]

# Function to remove profane words from vector of words
profan = function(.Words){
    # Find index
    .Words[!(.Words%in%profanityWords)]
}

# # (Very slow, ineffective, DO NOT USE) Function to remove profane words from lines 
# # only complete word matches (preceded and followed by a whitespace)
# pattern = paste0('\\s', profanity, '\\s')
# 
# profan = function(.Lines){
#     # Find index
#     mapply(function(.Line){
#         mapply(gsub, pattern = pattern, MoreArgs = list(x = .Line, perl = TRUE, 
#                                                         useBytes = TRUE,
#                                                         replacement = ''))
#     }, .Line = .Lines)%>%
#         unlist
# }
# 
# Function to remove lines containing profane words
pattern = paste0('\\s', profanity, '\\s')

profan = function(.Lines){
    # Find index of lines containing profane words
    profInd = mapply(grep, pattern = pattern,
                     MoreArgs = list(x = .Lines, perl = TRUE, useBytes = TRUE))%>%
        unlist
    .Lines[-profInd]
}

# Preprocessing the lines, Run the functions in appropriate order
Preprocess = function(Lines){
    Lines%>%
        punc%>%
        numb%>%
        profan%>% # Careful of the order
        words # Careful of the order
}

# Testing on twitter dataset
twitterWords = punc(twitter)
twitterWords = numb(twitterWords)
twitterWords = profan(twitterWords)
twitterWords = words(twitterWords)
length(twitterWords)
sum(nchar(twitterWords))



twitterWords = Preprocess(twitter)

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

# Function to form groups of words
wordGroups = function(words, group = 2){
    # Groups must be more than 2 words together
    if (group <2) stop("Cannot make groups of less than two words together")
    # Cannot make groups of fewer with words less than the group size
    if (length(words) < group)stop("The arguement 'words' must have length >= 1 group")
    # Create a matrix with each column containing a word of the group
    len = length(words)
    cols = character()
    for (grp in 1:group){
        cols = cbind(cols, words[grp:(len-group+grp)]) 
    }
    # Paste them together returning a vector with the groups in each row
    apply(cols, 1, paste, collapse = ' ')
}

twitterWordPairs = wordGroups(twitterWords, 3)
freq = as.data.frame(table(twitterWordPairs))
freq = freq[order(freq$Freq, decreasing = T),]%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))

# Plot it
ggplot(freq)+
    geom_line(aes(1:nrow(freq), CumFreq), size = 1, color = 'steelblue')+
    labs(x = 'Rank of word pairs by frequency',
         y = 'Cumulitive Frequency',
         title = 'Percent word pairs used in the text')
which.max(freq$CumFreq>0.5)
which.max(freq$CumFreq>0.9)

# With ngram package
twitterConcat = concatenate(twitterWords)
nG = ngram(twitterConcat, n = 3)
babble(nG, 100, seed = 123)