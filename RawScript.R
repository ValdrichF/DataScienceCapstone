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
twitter = readLines('./Data/en_US.twitter.txt')%>%
    sample(4000)
news    = readLines('./Data/en_US.news.txt')%>%
    sample(4000)
blogs   = readLines('./Data/en_US.blogs.txt')%>%
    sample(4000)

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

# Funtion to remove profanity words
profan = function(.Words){
    # text file with English Profanity words from
    # https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
    p = readLines('./Data/EnProfanity.txt')
    # Removing Profanity words
    .Words[-grep(paste('(', p, ')', collapse = '|', sep = ''),
                 .Words,
                 ignore.case = T,
                 perl = T)]
}

# Preprocessing the lines, Run the functions in appropriate order
Preprocess = function(Lines){
    Lines%>%
        punc%>%
        numb%>%
        words%>%
        profan

}

# Testing on twitter dataset
twitterWords = Preprocess(twitter)

twitterWords = punc(twitter)
twitterWords = numb(twitter)
twitterWords = words(twitter)
twitterWords = profan(twitter)


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
    cols = chracter(nrow(len-grp))
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

tic()
twitterWords = Preprocess(twitter)
toc()

PreprocessCmp = cmpfun(Preprocess)
tic()
twitterWords = PreprocessCmp(twitter)
toc()