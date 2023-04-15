# Libraries
library(readr)
library(tidyverse)
library(randomForestSRC)


# Load datasets
questions = read_csv('questions.csv')
clients = read_csv('clients.csv')


# Merge datasets on 'ClientUno'
colnames(questions)[which(colnames(questions)=="AskedByClientUno")]<-"ClientUno"

question_cli.df<-merge(questions, y = clients[ , c("ClientUno", "EthnicIdentity", 
                                                   "Age", "Gender", "MaritalStatus",
                                                   "NumberInHousehold", "AnnualIncome",
                                                   "AllowedIncome" )], 
                       by = "ClientUno", all.x=TRUE)


# Coerce columns to Numeric or Factor
question_cli.df$Age<-as.numeric(question_cli.df$Age)
question_cli.df$NumberInHousehold<-as.numeric(question_cli.df$NumberInHousehold)
question_cli.df$AnnualIncome<-as.numeric(question_cli.df$AnnualIncome)
question_cli.df$AllowedIncome<-as.numeric(question_cli.df$AllowedIncome)

question_cli.df$Category<-as.factor(question_cli.df$Category)
question_cli.df$EthnicIdentity<-as.factor(question_cli.df$EthnicIdentity)
question_cli.df$MaritalStatus<-as.factor(question_cli.df$MaritalStatus)
question_cli.df$Gender<-as.factor(question_cli.df$Gender)


# Filter data from Missouri
question_cli_mo.df <- question_cli.df %>% filter(StateAbbr=="MO")


# Create Factor 'FnC': Question in "Family and Children" category or not
question_cli_mo.df$FnC = question_cli_mo.df$Category=='Family and Children'

question_cli_mo.df$FnC = replace(question_cli_mo.df$FnC,
                                 question_cli_mo.df$FnC==TRUE, 1)
question_cli_mo.df$FnC = replace(question_cli_mo.df$FnC,
                                 question_cli_mo.df$FnC==FALSE, 0)
question_cli_mo.df$FnC = as.factor(question_cli_mo.df$FnC)


# Train/Test Split
set.seed(123)
n = nrow(question_cli_mo.df)
train = sample(1:n, n*0.7)
test = -(train)
mo.train = question_cli_mo.df[train,]
mo.test = question_cli_mo.df[test,]


# Random Forest Model (with runtime)
start.tm<-Sys.time()

rf.mo1<-rfsrc(FnC~EthnicIdentity+Age+MaritalStatus+Gender+
                NumberInHousehold+AnnualIncome+AllowedIncome, 
              data=mo.train)

end.tm<-Sys.time()
end.tm-start.tm


# Predictions on test set
predict(rf.mo1, mo.test)