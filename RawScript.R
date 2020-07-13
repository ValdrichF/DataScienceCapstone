library(dplyr)
library(stringr)
library(ggplot2)
library(ngram)
library(ggpubr)
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
rm(subInt)

# Function to remove punctions
punc = function(Lines){
    # add a symbol to show new line
    Lines = str_replace_all(Lines, '\\.|\\!|\\?', ' <n>')
    # Remove punctuations except contractions eg. can't
    str_replace_all(Lines, "(('t)|('ll)|('m)|('re)|('s)|('d)|('ve)|( <n>))|[[:punct:]]", "\\1")
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
profanityWords = profanity[-grep(' ', profanity)]%>%
    punc%>%
    numb

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
# # Function to remove lines containing profane words
# pattern = paste0('\\s', profanityWords, '\\s')
# 
# profan = function(.Lines){
#     # Find index of lines containing profane words
#     profInd = mapply(grep, pattern = pattern,
#                      MoreArgs = list(x = .Lines, perl = TRUE, useBytes = TRUE))%>%
#         unlist
#     .Lines[-profInd]
# }

# text file with English Profanity words from
# https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
profanity = readLines('./Data/EnProfanity.txt')
profanityWords = profanity[-grep(' ', profanity)]%>%
    punc%>%
    numb

# Faster function to remove profane words from vector of words
profan = function(.Words){
    # Find index
    .Words[!(.Words%in%profanityWords)]
}

# Preprocessing the lines, Run the functions in appropriate order
Preprocess = function(Lines){
    Lines%>%
        paste0(' <n>')%>%
        punc%>%
        numb%>%
        words%>% # Careful of the order
        profan # Careful of the order
}

# Testing on twitter dataset
twitterWords = punc(twitter)
twitterWords = numb(twitterWords)
twitterWords = profan(twitterWords)
twitterWords = words(twitterWords)
length(twitterWords)
sum(nchar(twitterWords))



twitterWords = Preprocess(twitter)
newsWords    = Preprocess(news)
blogWords    = Preprocess(blogs)

# Which words are the most frequent?
at = as.data.frame(table(twitterWords))
at = at[order(at$Freq, decreasing = T),]%>%
    filter(twitterWords != '<n>')%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))
an = as.data.frame(table(newsWords))
an = an[order(an$Freq, decreasing = T),]%>%
    filter(newsWords != '<n>')%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))
ab = as.data.frame(table(blogWords))
ab = ab[order(ab$Freq, decreasing = T),]%>%
    filter(blogWords != '<n>')%>%
    mutate(Freq = Freq/sum(Freq))%>%
    mutate(CumFreq = cumsum(Freq))

# How many unique words do you need in a frequency sorted dictionary to cover 50%
# of all word instances in the language? 90%?
t = ggplot(at)+
    geom_line(aes(1:length(twitterWords), CumFreq*100), size = 1, color = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
n = ggplot(an)+
    geom_line(aes(1:length(newsWords), CumFreq*100), size = 1, color = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
b = ggplot(ab)+
    geom_line(aes(1:length(blogWords), CumFreq*100), size = 1, color = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
annotate_figure(ggarrange(t, n, b, labels = c('Twitter', 'News', 'Blog'), ncol = 3, nrow = 1),
                bottom = text_grob('Rank of word by frequency'),
                left = text_grob('Cumulitive Frequency', rot = 90))
which.max(at$CumFreq>0.5)
which.max(at$CumFreq>0.9)

HistT = ggplot(head(at, 20))+
    geom_bar(aes(x = twitterWords, y = Freq), stat = 'Identity', fill = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
    ylim(c(0, 0.06))+
    scale_x_discrete(limits = at$twitterWords[1:20])
HistN = ggplot(head(an, 20))+
    geom_bar(aes(x = newsWords, y = Freq), stat = 'Identity', fill = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
    ylim(c(0, 0.06))+
    scale_x_discrete(limits = an$newsWords[1:20])
HistB = ggplot(head(ab, 20))+
    geom_bar(aes(x = blogWords, y = Freq), stat = 'Identity', fill = 'steelblue')+
    labs(x = '', y = '', title = '')+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
    ylim(c(0, 0.06))+
    scale_x_discrete(limits = ab$blogWords[1:20])
annotate_figure(ggarrange(HistT, HistN, HistB, labels = c('Twitter', 'News', 'Blog'), ncol = 3, nrow = 1),
                bottom = text_grob('20 most common words'),
                left = text_grob('Frequency', rot = 90))

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

twitterWordPairs = wordGroups(twitterWords, 2)
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

# funtion to predict the next word based on the 2 previous words
# consider using lapply and workign with multiple words of the line
# Have to remove punctuation and spaces
gram2 = function(w1, w2, dictionary = twitterWords, outsize = 3){
    w1 = tolower(w1)%>%
        str_replace_all( "(('t)|('ll)|('m)|('re)|('s)|('d)|('ve))|[[:punct:]]", "\\1")%>%
        str_replace_all('\\s','')
    w2 = tolower(w2)%>%
        str_replace_all( "(('t)|('ll)|('m)|('re)|('s)|('d)|('ve))|[[:punct:]]", "\\1")%>%
        str_replace_all('\\s','')
    ind1 = which(dictionary==w1)
    ind2 = which(dictionary==w2)
    pred1 = dictionary[ind1+1]
    pred2 = dictionary[ind2+2]
    prob1 = table(pred1)
    prob2 = table(pred2)
    row.names(prob1[order(prob1, decreasing = T)][1:10])
}

library(microbenchmark)

microbenchmark(gram2('the', 'in'))

twitterWords[ind1+1]


# With ngram package
twitterConcat = concatenate(twitterWords)
nG = ngram(twitterConcat, n = 3)
babble(nG, 100, seed = 123)