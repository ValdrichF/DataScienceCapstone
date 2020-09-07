library(quanteda)
# library(plyr)
library(dplyr)
library(stringr)
library(data.table)
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

files = list.files("./Data", pattern = "*.txt")
news = readr::read_lines('./Data/en_US.news.txt')
lines = length(news)
newsSub = news[sample.int(lines, lines*0.04)]
corp = corpus(newsSub)

# Insert start and end of sentence tokens (Pre-processing the words)
toks = tokens(corp, "sentence", remove_separators = T) # convert paragraphs/documents into sentences
toks = unlist(toks) # list to array
names(toks) = NULL  # remove document names (multiple rows with same name)
toks = tokens(toks, "word", remove_punct = T, remove_numbers = T,
              remove_symbols = T, remove_url = T)
toks = lapply(toks, function(x)append("<s>", append(x, "</s>")))
toks = as.tokens(toks)

# Function to pre-process the documents and return a token list
preProcess = function(documents){
    # convert paragraphs/documents into sentences
    toks = strsplit(documents, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)%>%
        unlist()%>%
        tolower()
    # Converting sentences to corpus
    toks = corpus(toks)
    # sentences to word tokens
    toks = tokens(toks, remove_punct = TRUE, remove_numbers = TRUE,
                  remove_symbols = TRUE, remove_url = TRUE)
    # add a start and end of sentence token
    toks = lapply(toks, function(x)append("<s>", append(x, "</s>")))
    # convert list to tokens
    as.tokens(toks)
}
toks = preProcess(newsSub)

# Function to create ngrams from tokens and find their counts
createNgrams = function(toks, n=2L){
    # Create a list of N-grams of word size n
    toksNgrams = tokens_ngrams(toks, n)
    # convert into character vector to use data.table functions
    b = data.table(ngram = unlist(toksNgrams, use.names = FALSE))
    # Find the count of every unique ngram
    b[,.(count = .N), by = ngram]
}

toks = preProcess(newsSub)

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

toksNgrams = foreach(i = 1:4, .packages = c("quanteda", "data.table", "stringr"))%dopar%
    createNgrams(toks = toks, n = i)

stopCluster(cl)

## Can create skip grams using tokens_ngrams(..., skip = "int")

# Join the n-1 gram for the denominator in the n gram

## Split the list into data tables
gram1 = toksNgrams[[1]]
gram2 = toksNgrams[[2]]
gram3 = toksNgrams[[3]]
gram4 = toksNgrams[[4]]

## Add columns for the n-1 gram to join the counts of ngram and n-1gram for scores
pattern = "^((.)|(<.\\w>))*_"
gram1 = gram1[, gram_1 := ngram]
gram2 = gram2[, ':='(gram_1 = str_remove(str_extract(ngram, pattern), "\\_$"),
                     prediction = str_remove_all(ngram, pattern))]
gram3 = gram3[, ':='(gram_1 = str_remove(str_extract(ngram, pattern), "\\_$"),
                     prediction = str_remove_all(ngram, pattern))]
gram4 = gram4[, ':='(gram_1 = str_remove(str_extract(ngram, pattern), "\\_$"),
                     prediction = str_remove_all(ngram, pattern))]

## Joining the two tables
setkey(gram1, ngram)
setkey(gram2, gram_1)
gram2Joined = gram1[gram2, .(i.ngram, i.gram_1, i.prediction, i.count, count)]

setkey(gram2, ngram)
setkey(gram3, gram_1)
gram3Joined = gram2[gram3, .(i.ngram, i.gram_1, i.prediction, i.count, count)]

setkey(gram3, ngram)
setkey(gram4, gram_1)
gram4Joined = gram3[gram4, .(i.ngram, i.gram_1, i.prediction, i.count, count)]

# Calculate the score for each ngram
threshold = 3
gram2Joined = gram2Joined[, score:=i.count/count*0.4*0.4]
gram3Joined = gram3Joined[, score:=i.count/count*0.4]
gram4Joined = gram4Joined[, score:=i.count/count]

## Removing na rows & rows with count less than the threshold
gram2Joined = gram2Joined[!is.na(count)&count>threshold,]
gram3Joined = gram3Joined[!is.na(count)&count>threshold,]
gram4Joined = gram4Joined[!is.na(count)&count>threshold,]

# Prune the model
## Remove the predictions which are not amoungst the top 5 predictions for the ngram
gram2JoinedPruned = gram2Joined[order(score, decreasing = TRUE), .SD[1:5], by = i.gram_1]
gram2JoinedPruned = gram2JoinedPruned[!is.na(score)]
gram3JoinedPruned = gram3Joined[order(score, decreasing = TRUE), .SD[1:5], by = i.gram_1]
gram3JoinedPruned = gram3JoinedPruned[!is.na(score)]
gram4JoinedPruned = gram4Joined[order(score, decreasing = TRUE), .SD[1:5], by = i.gram_1]
gram4JoinedPruned = gram4JoinedPruned[!is.na(score)]

## Consider representing the strings as integers in the data
##, might help with data size and reduce the number of times grep would be needed


## remove the rows where the n-1 gram score is more than the ngram


# Create the predicting function
preProcessPredict = function(documents){
    # convert paragraphs/documents into sentences
    toks = strsplit(documents, "(?<=[[:punct:]])\\\\s(?=[A-Z])", perl=T)%>%
        unlist()%>%
        tolower()%>%
        str_replace_all(pattern = "[[:punct:]]", "END")
    # Converting sentences to corpus
    toks = corpus(toks)
    # sentences to word tokens
    toks = tokens(toks, remove_numbers = TRUE, remove_punct = TRUE,
                  remove_symbols = TRUE, remove_url = TRUE)
    # add a start and end of sentence token,
    # lapply is a must as each element of the list should be a sentence
    toks = lapply(toks, function(x)append("<s>", str_replace_all(x, "END", "</s>")))
    # convert list to tokens
    unlist(toks, use.names = FALSE)
}
PredictText = function(string, bigram = gram2JoinedPruned, trigram = gram3JoinedPruned,
                       qgram = gram4JoinedPruned){
    string = preProcessPredict(string)
    strLength = length(string)
    gram2 = bigram[i.gram_1 == paste0(string[strLength], collapse = "_"),  .(i.prediction, score, i.ngram)]
    gram3 = trigram[i.gram_1 == paste0(string[-1:0+strLength], collapse = "_"), .(i.prediction, score)]
    gram4 = qgram[i.gram_1 == paste0(string[-2:0+strLength], collapse = "_"),   .(i.prediction, score)]
    hits  = bind_rows(gram4, gram3, gram2, .id = "gram")
    hits  = hits[ , .(score = max(score)), i.prediction]
    hits = hits[order(score, decreasing = TRUE),i.prediction][1:5] # ,i.prediction
    str_replace(hits[!is.na(hits[1:5])], "(<.\\w>)", ".")
}
microbenchmark(PredictText("I am studying! Data Science"))

# Run script for a subset of all data and group the ngrams together into 3 data


# skip-1 gram