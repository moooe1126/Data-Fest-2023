library(readr)
questionposts_MO <- read_csv("~/OneDrive - Southeast Missouri State University/Spring2023/Data Fest 2023/Data/questionposts_MO.csv")
table(questionposts_MO$Category)

library(dplyr)
Financial <- questionposts_MO %>%
  filter(Category == "Consumer Financial Questions")
FamilyChildren <- questionposts_MO %>%
  filter(Category == "Family and Children")
IndividualRights <- questionposts_MO %>%
  filter(Category == "Individual Rights")
Other <- questionposts_MO %>%
  filter(Category == "Other")
Education <- questionposts_MO %>%
  filter(Category == "Education")
HealthDisability <- questionposts_MO %>%
  filter(Category == "Health and Disability")
IncomeMaintenance <- questionposts_MO %>%
  filter(Category == "Income Maintenance")
Juvenile <- questionposts_MO %>%
  filter(Category == "Juvenile")
WorkRelated <- questionposts_MO %>%
  filter(Category == "Work, Employment and Unemployment")
HousingHomelessness <- questionposts_MO %>%
  filter(Category == "Housing and Homelessness")


library(tidytext)
data(stop_words)



library(tm)
clean_tx<-function(data){
data(stop_words)

b <- tolower(data) # Convert the text to lowercase
c <- removeWords(b, stop_words$word) # Remove stop words
d <- gsub('[[:punct:]]+', ' ', c) # Remove punctuation
#e<-str_split(d," ")
x<-unlist(d)

return(x)
}




x<-sapply(Other$PostText,clean_tx)


x <- VectorSource(x)
x <- VCorpus(x)

dtm <- TermDocumentMatrix(x)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- d %>%
  filter(word != "attorney" & word != "lawyer" & word != "court" & word != "missouri" & word != "legal")

#install.packages("wordcloud")
#library(wordcloud)
library(ggplot2)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,scale=c(3.5,.2),
          max.words=150, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(8, "Dark2"))
mtext("title", side=1)

