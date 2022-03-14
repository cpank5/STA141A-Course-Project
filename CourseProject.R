
#libraries need to access
library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(class)
library("kernlab")
library(car)
library(ggeffects)


## Preprocessing #### 
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv",lazy = FALSE)
write_csv(covid,file="WHO-Covid-19-backup.csv")


covid <- covid %>% 
  filter(WHO_region != "Other") %>% 
  mutate(WHO_region = fct_recode(WHO_region,                            "Eastern Mediterranean"="EMRO","Europe" = "EURO","Africa" = "AFRO","Western Pacific" = "WPRO","Americas"="AMRO","South-East Asia" = "SEARO"))

covid$death_rate = covid$New_deaths/covid$Cumulative_deaths
covid_filtered = covid %>% filter(is.na(death_rate) !=TRUE) #get rid of NAs will skew dates however

country_d=covid%>%
  group_by(Country)%>%
  summarize(max_cases=max(Cumulative_cases, na.rm = TRUE),
            max_deaths = max(Cumulative_deaths),
  )
country_d$death_rate = (country_d$max_deaths/country_d$max_cases)*100

country_d %>% filter(is.na(death_rate) !=TRUE) #get rid of NA countries

country_class = read.csv("fin_country_class.csv")
death_class =  inner_join(country_d,country_class , by = c("Country")) #merge two datasets
death_class = death_class %>% filter(is.na(death_rate) !=TRUE) 
```
```{r}
#Sort death_class by population of developed and developing
developed_countries = death_class %>% filter(Classification=="Developed") %>% arrange(Population..mil.)
developing_countries = death_class %>% filter(Classification=="Developing")  %>% arrange(Population..mil.)

countries_large = rbind(developed_countries[24:46,], developing_countries[92:181,]) #combine half the developed and developing into one
countries_large$case_rate = ((countries_large$max_cases/1000000)/countries_large$Population..mil.)*100 #Scale max cases to match the millions of population and get the percentage of people testing postive in the country over the course of the pandemic
head(countries_large)


#training and test set
## 75% of the sample size
smp_size <- floor(0.80 * nrow(countries_large))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(countries_large)), size = smp_size)

train <- countries_large[train_ind, ]
test <- countries_large[-train_ind, ]

###Descriptive Analysis
summary.developing = developing_countries %>% 
  summarise(mean_score=mean(death_rate)) # You can use other summary statistics like median, mean, min, max depends on what sum stats you want
summary.developed = developed_countries %>% 
  summarise(mean_score=mean(death_rate))

ggplot(data=countries_large, mapping=aes(x=Classification, y=death_rate)) +
  geom_boxplot() +
  xlab("Classification")

summary.developing
summary.developed

ggplot(data=countries_large, mapping=aes(x= Classification, y=case_rate)) +
  geom_boxplot() +
  xlab("Classification")

ggplot(data=countries_large, mapping=aes(x= Region, y=death_rate)) +
  geom_boxplot() +
  xlab("Region")

ggplot(data=countries_large, mapping=aes(x= Region, y=case_rate)) +
  geom_boxplot() +
  xlab("Region")

### Inferential analysis
countries_anova = countries_large[-c(75,88),] #remove significant outliers of Yemen and Sudan
model1 <- aov(death_rate ~ Classification + Region  , data = countries_anova)
Anova(model1, type = "II")

countries_anova = countries_large[-c(75,88),] #remove significant outliers of Yemen and Sudan
model1 <- aov(death_rate ~ Classification + Region  , data = countries_anova)
sig.level =0.05;
T.ci=TukeyHSD((model1), conf.level= 1-sig.level)
par(mfrow=c(1,1))
plot(T.ci, las=1, col="blue")
par(mfrow=c(1,1))

#Logistic regression with death and case rate
mod2<-glm(Classification ~ death_rate + case_rate + death_rate:case_rate, data=train, family="binomial")
summary(mod2)

plot(ggpredict(mod2,"death_rate [all]"))
plot(ggpredict(mod2,"case_rate [all]"))

class.pred.glm.prob = predict(mod2, test, type = "response")


class.pred.glm_label = ifelse(class.pred.glm.prob > 0.5, TRUE, FALSE)

# Create the confusion matrix by tabulating true classes against predicted classes.
class.confusion.glm = table(true = test$Classification, predicted = class.pred.glm_label)
class.confusion.glm
# Compute misclassification error rate
class.pred.glm_error = (class.confusion.glm[1,2] + class.confusion.glm[2,1])/sum(class.confusion.glm)
class.pred.glm_error

knn_mod1<-knn(matrix(train$death_rate,ncol=1), matrix(test$death_rate,ncol=1), train$Classification, k = 10, prob=FALSE)
knn_mod1

confusion.knn1 = table(true = test$Classification, predicted = knn_mod1)
confusion.knn1

# Compute misclassification error rate
knn1_error = (confusion.knn1[1,2] + confusion.knn1[2,1])/sum(confusion.knn1)
knn1_error

#kmeans clustering
countries.k2 <- countries_large[,4,9,6] %>%  kmeans(2)

countries_large %>% mutate(cluster = countries.k2$cluster) %>%
  ggplot(aes(x=death_rate, y=case_rate, color = as.factor(cluster))) + 
  geom_point() +
  scale_fill_discrete(name = "Classification", labels = c("Developed", "Developing"))

countries.k2 <- countries_large[,4,9,6] %>%  kmeans(2)

countries_large %>% mutate(cluster = countries.k2$cluster) %>%
  ggplot(aes(x=death_rate, y=case_rate, color = Classification)) + 
  geom_point()

countries.k2 <- countries_large[,4,9,6] %>%  kmeans(2)

countries_large %>% mutate(cluster = countries.k2$cluster) %>%
  ggplot(aes(x=death_rate, y=case_rate, color = Region)) + 
  geom_point()

###Sensitivity Analysis
plot(model1)

# Levene's test with multiple independent variables
leveneTest(death_rate ~ Classification*Region, data = countries_large)



