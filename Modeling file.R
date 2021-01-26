library(tidyverse)

train<-read.csv('Train.csv')
test<-read.csv('Test.csv')
train<-train %>% select(subset=-c(X,profit_group))
test<-test %>% select(subset=c(-X))
fit<-glm(formula=order_status~gender_Male+
         wealth_segment_High.Net.Worth+wealth_segment_Mass.Customer+owns_car_Yes+state_QLD+state_VIC+
         age_group_30.45+age_group_45.60+age_group_60.75+age_group_75.90, data = train, family = binomial())
summary(fit)
final<-predict(fit, test)
test_predictions <- as.data.frame(final)

head(test_predictions)
mean(test_predictions$final)
test_predictions$final<-ifelse(test_predictions$final>4.9, 1, 0)
head(test_predictions)

newcust<-read.csv('KPMG_newcust.csv')
newcust<-newcust[1:983,]

test_final<-data.frame(customerfst = newcust$first_name, customerlst = newcust$last_name, sale=test_predictions$final)

head(test_final)

write.csv(test_final, "sale_status.csv")
