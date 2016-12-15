#---
#  title: "StackSenti"
#author: "Alok Satpathy"
#date: "December 9, 2016"
#output: word_document
#---
  
#  ```{r}
library(devtools)
library(sentiment)
library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(stringr)


#Invoke required libraries - to be filtered
getwd()
setwd("C:/Users/Alok Satpathy/Desktop/Fall 2016/EDA/Final Project")

Q3csv=read.csv("Question3Dataset.csv")
attach(Q3csv)
str(Q3csv)

Q3csv[is.na(Q3csv)]=0
str(Q3csv)


Q3csv["SentimentScore"]=NA
str(Q3csv)


#function to clean data
cleancomments = function(commentstest)
{
  commentstest_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",commentstest)
  commentstest_cl = gsub("http[^[:blank:]]+", "", commentstest_cl)
  commentstest_cl = gsub("@\\w+", "", commentstest_cl)
  commentstest_cl = gsub("[ \t]{2,}", "", commentstest_cl)
  commentstest_cl = gsub("^\\s+|\\s+$", "", commentstest_cl)
  commentstest_cl = gsub("[[:punct:]]", " ", commentstest_cl)
  commentstest_cl = gsub("[^[:alnum:]]", " ", commentstest_cl)
  commentstest_cl <- gsub('\\d+', '', commentstest_cl)
  return(commentstest_cl)
}


#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- lapply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word
    sentence = cleancomments(sentence)
    sentence <- tolower(sentence)
    wordList <- strsplit(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

#load pos,neg statements
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
summary(afinn_list$score)
afinn_list$word <- tolower(afinn_list$word)
#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- afinn_list$word[afinn_list$score==-2 | afinn_list$score==-1 | afinn_list$score==-3]
posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1]
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]  



for(i in 1:nrow(Q3csv))
{
  rowSenti=as.data.frame(sentimentScore(Q3csv[i,"CommentText"], vNegTerms, negTerms, posTerms, vPosTerms))
  #print(rowSenti)
  totalSenti=((as.numeric(as.character(rowSenti[,2])))*(-2)) + ((as.numeric(as.character(rowSenti[,3])))*(-1)) + ((as.numeric(as.character(rowSenti[,4])))*(1)) + ((as.numeric(as.character(rowSenti[,5])))*(2))
  #print(totalSenti)
  
  if(totalSenti>0)
  {
    totalSenti=1
  }
  else if(totalSenti<0)
  {
    totalSenti=-1
  }
  
  Q3csv[i,"SentimentScore"]=totalSenti
  
  #Uncomment to calculate polarity -- very slow process
  #polar=sentiment(Q3csv[i,"CommentText"])
  #Q3csv[i,"SentimentPolarity"]=polar[1,"polarity"]
  
  if(i%%1000 == 0) 
  {
    cat(i, " Sentiments Analyzed\n")
  }
}


attach(Q3csv)
Q3df=data.frame(CommentScore, CommentCrDays, CommentLength, comment_owner_reputation, comment_owner_profile_summary, comment_owner_views, comment_owner_upvotes, comment_owner_downvotes, comment_owner_lastactivity_days, editDurationAfterCreation, activityDurationAfterCreation, title_length, num_tags, PostAnswerCount, num_favorite, hascode, post_views, postTypeId, IsAcceptedAnswer, postScore, post_length, PostCommentCount, PostUpVotes, PostDownVotes, SentimentScore)

Q3df$SentimentScore <- as.factor(Q3df$SentimentScore)
str(Q3df)



Q3smp_size=floor(0.75*nrow(Q3df))
set.seed(123)
Q3train_ind=sample(seq_len(nrow(Q3df)), size = Q3smp_size)
Q3Train=Q3df[Q3train_ind, ]
Q3Test=Q3df[-Q3train_ind, ]

nrow(Q3Train)
nrow(Q3Test)

Q3lm=glm(SentimentScore~.,data=Q3Train,family="binomial")


summary(Q3lm)

count(Q3Train, postTypeId)


