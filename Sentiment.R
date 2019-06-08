pos.words <- read.csv("pos.csv")
neg.words <- read.csv("neg.csv")

pos.words <- scan("pos.csv",what = 'character')
neg.words <- scan("neg.csv",what = 'character')

pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

test <- ldply(insta,function(t) t$toDataFrame() )
result <- score.sentiment(test$text,pos.words,neg.words)

summary(result$score)
hist(result$score,col ="yellow", main ="Score of tweets", ylab = " Count of tweets")

count(result$score)

library(ggplot2)
qplot(result$score,xlab = "Score of tweets")
