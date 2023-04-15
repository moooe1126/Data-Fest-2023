library(readr)
questions <- read_csv("Data/questions.csv")
attorneys <- read_csv("Data/attorneys.csv")

library(skimr)
library(dplyr)
library(tidyr)

MO <- attorneys[, c(2, 3)] %>%
  filter(StateAbbr == "MO")

colnames(questions)[10]<-'AttorneyUno'
questions <- questions[, c(2, 5,7, 10)]


df <- merge(MO, questions, by = "AttorneyUno", all.x=T,  all.y = T)
df <- df %>%
  filter(!is.na(StateAbbr.x))


df_a <- df[,-3] %>%
  group_by(AttorneyUno, Category)  %>%
  count()


Category_top10 <- df_a %>%
  group_by(Category)  %>%
  arrange(desc(n))%>%
  slice(1:10)
Category_top10 <- Category_top10[-c(51:55),]

write.csv(Category_top10, file = "Category_top10_AttorneysMO")



attorneys <- attorneys[, c(2, 3)]
questions_count <- questions %>%
  group_by(AttorneyUno, Category)  %>%
  count()
df <- merge(MO, questions, by = "AttorneyUno", all.x=T,  all.y = T)




library(readr)
clients <- read_csv("~/OneDrive - Southeast Missouri State University/Spring2023/Data Fest 2023/Data/clients.csv")
table(clients$StateAbbr)

