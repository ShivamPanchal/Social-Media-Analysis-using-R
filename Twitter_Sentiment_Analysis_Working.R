##Author: Shivam Panchal, Decision Science Enthusiast
##Project Title: Twitter Sentiment Analysis

## INSTALLING PACKAGES THAT WE WILL USE TODAY
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(openssl)
library(httpuv)

#####################################
### CREATING YOUR OWN OAUTH TOKEN ###
#####################################


## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com)
##			(make sure you leave 'Callback URL' empty)
## Step 4: Agree to user conditions
## Step 5: copy consumer key and consumer secret and paste below

setup_twitter_oauth(consumer_key='FEBIYlhPxYQ7aw0X9DipKbpM6',
                    consumer_secret='bJaGG7yQGgt3ec2yzXJZT1U7vU42tBjkj5nK8NDpf4YsISXjYy',
                    access_token = '771287280438968320-O1moB8waFZCG4DTCqfqPhQeoLrPIslj',
                    access_secret = 'iW8wkoIVPDdNqRWvUHrxAKJaYbDj7IOpiWqk5KMoPNmN3')


#####################################
### COLLECTING USER INFORMATION   ###
#####################################
user <- getUser('barackobama')
user$followersCount
user$getFollowers(n=10)


findme<-getUser('Shivam_pancha1')
findme
findme$description

findme$getFollowers()



## Continuing the Project.....




tweets = searchTwitter("#SachinTendulkar", n=2000)
length(tweets)

Tweets.text = laply(tweets,function(t)t$getText())
Tweets.text
pos = scan('E:\\Data Science\\R\\Social Media Project\\positive-words.txt', what='character', comment.char=';')
neg = scan('E:\\Data Science\\R\\Social Media Project\\negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score = sum(pos.matches) - sum(neg.matches)
    #score <- rbind(pos.matches,neg.matches)
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}
analysis = score.sentiment(Tweets.text, pos, neg)
table(analysis$score)
hist(analysis$score)

#Histogram - iphone7
hist(analysis$score,xlab="Twitter Tweets",main="Analysis of Tweets",col="darkblue",xlim=c(-2,4),ylim=c(0, 2000))
#Histogram - Sachin Tendulkar
hist(analysis$score,xlab="Twitter Tweets",main="Analysis of Tweets",col="darkblue",xlim=c(-2,4),ylim=c(0, 190))