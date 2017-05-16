#################################
# Install necessary libray
install.packages("ROCR")
install.packages("caret")
install.packages("e1071")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("mapproj")

#################################
# Load packages
library(ROCR)
library(caret)
library(e1071)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
##################################
#cleaning data sets
#a) loan_status2
#     Fully Paid 1
#     Charged Off 2
#     Does not meet the credit policy. Status:Charged Off 3
#     Does not meet the credit policy. Status:Fully Paid 4
#
#b) verification_status2
#     Verified 0
#     Source Verfied 0
#     Not Verified 1
#
#c) purpuse2
#car 0
#credit_card 1
#debt_consolidation 2
#educational 3
#home_improvements 4
#house 5
#major_purchase 6
#medical 7
#moving 8
#renewable_energy 9
#small_business 10
#vacation 11
#wedding 12
#other

##################################
# Loading dataset into R
library(readr)
loan <- read_csv("loan3.csv")
str(loan)


##################################
#Finding and plotting correlation between annual_inc and addr_state
table1<-tapply(loan$annual_inc,loan$addr_state,mean,na.rm=TRUE)
table_name=names(table1)
plot(table1,xaxt="n",main="Relation between annual income of states",xlab="US states", ylab="Avg Annual Income")
axis(1,at=1:length(table_name),labels=table_name)

#Analyis : Folks from higher income states also takes more loans 

##################################
#
#Data Division
training.index <- sample(1:nrow(loan), 20313, replace = FALSE) 
training.set <- loan[training.index,]
test.set <- loan[-training.index,]

##################################

#creating glm model to predict loan_status2 from verification_status2 in loan
model <- glm(loan_status2~verification_status2, data=loan, family = binomial)

#Prediction on model
scores <- predict(model, newdata = test.set, type= "response")
#pr <- prediction(scores, labels= test.set$verification_status2)
pred.fit.glm <- ifelse(scores>0.5, 0, 1)
confusionMatrix(pred.fit.glm, test.set$loan_status2, positive = "1")

#Accuracy : 78.3%
###########################################################

#creating glm model to predict loan_status2 from purpose2 in loan
model <- glm(loan_status2~annual_inc, data=loan, family = binomial)

#Prediction on model
scores <- predict(model, newdata = test.set, type= "response")
pred.fit.glm <- ifelse(scores>0.5, 1, 0)
confusionMatrix(pred.fit.glm, test.set$loan_status2, positive = "1")

#Accuracy : 84%
########################################################

#tapply(loan$int_rate1,loan$addr_state,mean)


#################################333
# ggplot using open source code 
us <- map_data("state")

arr <- loan %>% 
  add_rownames("region") %>% 
  mutate(region=tolower(loan$addr_state_v2))

gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=arr, map=us,
                    aes(fill=loan$loan_amnt, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='#ffffff', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg

