# Loading libraries using lapply function
libs <- c("RSQLite","sqldf","tm","wordcloud")
lapply(libs, require, character.only = TRUE)

# You can choose either way to importing data

#1st way
# Opens a Connection to a database in given path i.e. "database.sqlite"
db <- dbConnect(dbDriver("SQLite"),"../input/database.sqlite")
reviews <- dbGetQuery(db,"select * from Reviews limit 10000")

#2nd way
#reviews <- read.csv("../input/Reviews.csv",stringsAsFactors = FALSE)

# Wrote a function which cleans data, calculates no. of words and frequency and gives input 
# to wordcloud function which generates an image. 
wc <- function(documents){
  corpusnew <- Corpus(VectorSource(documents))
  corpusnew <- tm_map(corpusnew,content_transformer(tolower))
  corpusnew <- tm_map(corpusnew,removePunctuation)
  corpusnew <- tm_map(corpusnew,stripWhitespace)
  corpusnew <- tm_map(corpusnew,removeWords,stopwords("english"))
  
  freq <- DocumentTermMatrix(corpusnew)
  word_freq <- as.data.frame(as.matrix(freq))
  words <- colnames(word_freq)
  freq <- colSums(word_freq)
  
  wordcloud(words,freq,min.freq = sort(freq, decreasing = TRUE)[[100]], 
            colors=brewer.pal(8,"Dark2"),random.color=TRUE)
}

#Saving image in png format
png("wordcloud.png")
wc(reviews$Text)
dev.off()
