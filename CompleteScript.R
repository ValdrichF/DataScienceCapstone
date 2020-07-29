library(dplyr)
library(stringr)
library(ggplot2)
library(ngram)
library(ggpubr)
library(tidyr)
library(data.table)
library(plyr)
library(doParallel)

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

# Function to remove punctions
punc = function(Lines){
    # add a symbol to show new line
    Lines = str_replace_all(Lines, '[\\.\\!\\?]+', ' <n> ')
    # Remove punctuations except contractions eg. can't
    Lines = str_replace_all(Lines, "(((?<=\\w)'t)|((?<=\\w)'ll)|((?<=\\w)'m)|((?<=\\w)'re)|((?<=\\w)'s)|((?<=\\w)'d)|((?<=\\w)'ve)|(<n>))|[[:punct:]]", "\\1")
    # Make sure every line ends with a <n>
    badEnd = str_which(Lines, "<n>\\s?$", TRUE)
    Lines[badEnd] = paste(Lines[badEnd], '<n>')
    # Final attempt to remove stray punctuations
    gsub("\\s+\\W\\s+", " ", Lines, perl = TRUE)
    
}

# Remove numbers
numb = function(Lines){
    str_remove_all(Lines, '\\d')
}

# Function to split the strings into words
words = function(.line){
    x = tolower(.line)%>%
        str_split(pattern = '( <n> )', simplify = TRUE)%>%
        t%>%
        as.vector%>%
        str_split(pattern ='((?<=.)\\s+(?=.))')%>%
        # unlist%>%
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
rm(profanity)

# Faster function to remove profane words from vector of words
profan = function(.Words){
    # Find index
    .Words[!(.Words%in%profanityWords)]
}

# Preprocessing the lines, Run the functions in appropriate order
Preprocess = function(Lines){
    Lines%>%
        tolower%>%
        numb%>%
        punc%>%
        # words%>% # Careful of the order
        str_replace_all("\\s+", " ")
}


CreateFreqTable = function(string, n=5){
    string[str_count(string, "\\S\\s+\\S")>(n-2)]%>%     # Check to make sure the right number of words
        ngram(n = n)%>%   # Create a 5gram
        get.phrasetable%>%# phrase table consists of the frequency and proportion of the ngrams
        mutate(ngrams = str_trim(ngrams, side = "both"))%>%# Remove extra space at the start and end of the ngrams
        select(-prop) # Remove the proportion column from the phrase table
}

CalculateScore = function(freqN, freqN1){
    freqN1 = mutate(freqN1, gram = str_remove_all(ngrams, " [[:alpha:]]*$"))%>%
        mutate(prediction = str_trim(str_extract(ngrams, " [[:alpha:]]*$")))
    left_join(freqN1, freqN, by = c("gram" = "ngrams"), suffix = c(".n1", ".n"))%>%
        filter(freq.n>countThresh)%>%
        mutate(score = freq.n1/freq.n)%>%
        filter(score<=1&!is.na(score))
}

# Importing a 'random' sample of the datasets of size 4000
# twitter = readLines('./Data/en_US.twitter.txt')
# news    = readLines('./Data/en_US.news.txt', encoding = "UTF-8", skipNul = TRUE)
# blogs   = readLines('./Data/en_US.blogs.txt')

countThresh = 1

# read all lines into a vector
news = readr::read_lines('./Data/en_US.news.txt')
news = Preprocess(news)

# Split into a list 
news = split(news, rep(1:100, length.out = length(news)))

# use ldply to call CreateFreqTable function
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

news5gram = ldply(news, CreateFreqTable, n=5, .parallel = TRUE,
                  .paropts = list(.export = "news", .packages = c("stringr", "dplyr" , "ngram")))
saveRDS(news5gram, "./Results/news5gram.rds")
rm(news5gram)

news4gram = ldply(news, CreateFreqTable, n=4, .parallel = TRUE,
                  .paropts = list(.export = "news", .packages = c("stringr", "dplyr" , "ngram")))
saveRDS(news4gram, "./Results/news4gram.rds")
rm(news4gram)

news3gram = ldply(news, CreateFreqTable, n=3, .parallel = TRUE,
                  .paropts = list(.export = "news", .packages = c("stringr", "dplyr" , "ngram")))
saveRDS(news3gram, "./Results/news3gram.rds")
rm(news3gram)

news2gram = ldply(news, CreateFreqTable, n=2, .parallel = TRUE,
                  .paropts = list(.export = "news", .packages = c("stringr", "dplyr" , "ngram")))
saveRDS(news2gram, "./Results/news2gram.rds")
rm(news2gram)

news1gram = ldply(news, function(x){
    str_split(x, "\\s+")%>% # Split the cleaned twitter into words
        unlist%>% # Convert to character array
        table%>%  # Frequency of the words
        as.data.frame%>%
        rename(freq = Freq, ngrams = 1)%>% # match the naming scheme in the phrasetable
        mutate(ngrams = as.character(ngrams))%>%
        filter(ngrams != "") # Remove empty character cells
},.parallel = TRUE, .paropts = list(.export = "news", .packages = c("stringr", "dplyr")))
saveRDS(news1gram, "./Results/news1gram.rds")
rm(news1gram)

stopCluster(cl)
repeat {
    twitter = readLines(con, n = 1010L, encoding = "UTF-8", skipNul = TRUE)
    if (length(twitter)==0){
        # disconnect the connection
        close(con)
        #break the loop
        break
    }
    twitterWords = Preprocess(twitter)
    rm(twitter)
    
    twitter5gram = CreateFreqTable(twitterWords, 5)
    twitter4gram = CreateFreqTable(twitterWords, 4)
    twitter3gram = CreateFreqTable(twitterWords, 3)
    twitter2gram = CreateFreqTable(twitterWords, 2)
    
    twitter1gram = str_split(twitterWords, "\\s+")%>% # Split the cleaned twitter into words
        unlist%>% # Convert to character array
        table%>%  # Frequency of the words
        as.data.frame%>%
        rename(freq = Freq, ngrams = 1)%>% # match the naming scheme in the phrasetable
        mutate(ngrams = as.character(ngrams))%>%
        filter(ngrams != "") # Remove empty character cells
    
    twitter5gramConcat = rbind.data.frame(twitter5gramConcat, twitter5gram)
    twitter4gramConcat = rbind.data.frame(twitter4gramConcat, twitter4gram)
    twitter3gramConcat = rbind.data.frame(twitter3gramConcat, twitter3gram)
    twitter2gramConcat = rbind.data.frame(twitter2gramConcat, twitter2gram)
    
    cat('\r', paste('Trained', i, 'lines from twitter'))
    i = i+1010L
}

twitter5gramConcat = setDT(twitter5gramConcat)
twitter5gramConcat = twitter5gramConcat[,.(freq = sum(freq)), "ngrams"]
twitter4gramConcat = setDT(twitter4gramConcat)
twitter4gramConcat = twitter4gramConcat[,.(freq = sum(freq)), "ngrams"]
twitter3gramConcat = setDT(twitter3gramConcat)
twitter3gramConcat = twitter3gramConcat[,.(freq = sum(freq)), "ngrams"]
twitter2gramConcat = setDT(twitter2gramConcat)
twitter2gramConcat = twitter2gramConcat[,.(freq = sum(freq)), "ngrams"]

if(!dir.exists("./Results"))dir.create("./Results")
write.csv(twitter2gramConcat, "./Results/news2gram.csv")
write.csv(twitter3gramConcat, "./Results/news3gram.csv")
write.csv(twitter4gramConcat, "./Results/news4gram.csv")
write.csv(twitter5gramConcat, "./Results/news5gram.csv")

twitter5gram = CreateFreqTable(twitterWords, 5)
twitter4gram = CreateFreqTable(twitterWords, 4)
twitter3gram = CreateFreqTable(twitterWords, 3)
twitter2gram = CreateFreqTable(twitterWords, 2)

twitter1gram = str_split(twitterWords, "\\s+")%>% # Split the cleaned twitter into words
    unlist%>% # Convert to character array
    table%>%  # Frequency of the words
    as.data.frame%>%
    rename(freq = Freq, ngrams = 1)%>% # match the naming scheme in the phrasetable
    mutate(ngrams = as.character(ngrams))%>%
    filter(ngrams != "") # Remove empty character cells

# Calculating the scores from ngram count
countThresh = 1

twitter5gram = CalculateScore(twitter4gram, twitter5gram)
twitter4gram = CalculateScore(twitter3gram, twitter4gram)
twitter3gram = CalculateScore(twitter2gram, twitter3gram)
twitter2gram = CalculateScore(twitter1gram, twitter2gram)

twitter5gram = mutate(twitter5gram, gram = str_remove_all(ngrams, " [[:alpha:]]*$"))%>%
    mutate(prediction = str_trim(str_extract(ngrams, " [[:alpha:]]*$")))
twitter5gram = left_join(twitter5gram, twitter4gram, by = c("gram" = "ngrams"), suffix = c(".5", ".4"))%>%
    filter(freq.4>countThresh)%>%
    mutate(score = freq.5/freq.4)%>%
    filter(score<=1&!is.na(score))

twitter4gram = mutate(twitter4gram, gram = str_remove_all(ngrams, " [[:alpha:]]*$"))%>%
    mutate(prediction = str_trim(str_extract(ngrams, " [[:alpha:]]*$")))
twitter4gram = left_join(twitter4gram, twitter3gram, by = c("gram" = "ngrams"), suffix = c(".4", ".3"))%>%
    filter(freq.3>countThresh)%>%
    mutate(score = freq.4/freq.3)%>%
    filter(score<=1&!is.na(score))

twitter3gram = mutate(twitter3gram, gram = str_remove_all(ngrams, " [[:alpha:]]*$"))%>%
    mutate(prediction = str_trim(str_extract(ngrams, " [[:alpha:]]*$")))
twitter3gram = right_join(twitter3gram, twitter2gram, by = c("gram" = "ngrams"), suffix = c(".3", ".2"))%>%
    filter(freq.2>countThresh|is.na(freq.3))%>%
    mutate(score = freq.3/freq.2)%>%
    filter(score<=1&!is.na(score))

a  = str_split(twitter2gram$ngrams, "\\s+", n = 2, simplify = TRUE)
twitter2gram = mutate(twitter2gram, gram = a[,1])%>%
    mutate(prediction = a[,2])
twitter2gram = left_join(twitter2gram, twitter1gram, by = c("gram" = "ngrams"), suffix = c(".2", ".1"))%>%
    filter(freq.1>countThresh)%>%
    mutate(score = freq.2/freq.1)%>%
    filter(score<=1&!is.na(score))

twitter5gram = select(twitter5gram, gram, prediction, score)
twitter4gram = select(twitter4gram, gram, prediction, score)
twitter3gram = select(twitter3gram, gram, prediction, score)
twitter2gram = select(twitter2gram, gram, prediction, score)

# Create a unifying scorebook - 2 columns of words (preceeding words + prediction) and the scores
jointDS = list(setDT(twitter5gram),
               setDT(twitter4gram),
               setDT(twitter3gram),
               setDT(twitter2gram))

# P(continuous)- to handel unknown words

# Final model

ngramStupid = function(string){
    # Convert string into appropriate format
    string = Preprocess(string)%>%
        str_split("\\s+", simplify = TRUE)
    len = length(string)-1
    string = string[-(len+1)]
    # check for short strings
    
    # loop to find the best predictions for each ngram, dropping to a lower ngram if first loop is not satisfied
    Pred = data.table(prediction = character(), score = integer())
    DSInd = 1
    if (DSInd<=4){ #nrow(Pred)<3&
        # Create temporary version of string with required length (if is required)
        str = paste(string[(len-4+DSInd):len], collapse = ' ')
        # Subset of the data set where the gram matches the given string
        DSSub = jointDS[[DSInd]][gram==str]
        Pred = rbind(Pred, DSSub[, .(prediction, score*0.4^(DSInd-1))], use.names = FALSE)
        DSInd = DSInd+1
    }
    # return the predictions
    Pred = Pred[, .(score = sum(score)), prediction]
    Pred[order(-score)][1:3]
}
