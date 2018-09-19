getwd()
setwd("/home/administrator/Documents/")
library(ggplot2)
library(gridExtra)
library(devtools)
library(corrgram)
library(sampling)
library(caret)
library(rpart)
library(scales)
rental_bikes_data=read.csv("day.csv")
View(rental_bikes_data)
#correcting the data format
rental_bikes_data$casual=NULL
rental_bikes_data$registered=NULL
rental_bikes_data$dteday=NULL
rental_bikes_data$season[rental_bikes_data$season %in% "1"] = "springer"
rental_bikes_data$season[rental_bikes_data$season %in% "2"] = "summer"
rental_bikes_data$season[rental_bikes_data$season %in% "3"] = "fall"
rental_bikes_data$season[rental_bikes_data$season %in% "4"] = "winter"
rental_bikes_data$yr[rental_bikes_data$yr %in% "0"] = "2011"
rental_bikes_data$yr[rental_bikes_data$yr %in% "1"] = "2012"
rental_bikes_data$holiday[rental_bikes_data$holiday %in% "1"]="yes"
rental_bikes_data$holiday[rental_bikes_data$holiday %in% "0"]="no"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "0"] = "sun"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "1"] = "mon"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "2"] = "tue"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "3"] = "wed"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "4"] = "thu"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "5"] = "fri"
rental_bikes_data$weekday[rental_bikes_data$weekday %in% "6"] = "sat"
rental_bikes_data$weathersit[rental_bikes_data$weathersit %in% "1"] = "clear or partly cloudy"
rental_bikes_data$weathersit[rental_bikes_data$weathersit %in% "2"] = "mist or few clouds"
rental_bikes_data$weathersit[rental_bikes_data$weathersit %in% "3"] = "light snow or rain"
rental_bikes_data$weathersit[rental_bikes_data$weathersit %in% "4"] = "heavy rain or thunderstorm"
str(rental_bikes_data)
rental_bikes_data$mnth=as.factor(rental_bikes_data$mnth)
rental_bikes_data$season=as.factor(rental_bikes_data$season)
rental_bikes_data$holiday=as.factor(rental_bikes_data$holiday)
rental_bikes_data$yr=as.factor(rental_bikes_data$yr)
rental_bikes_data$weekday=as.factor(rental_bikes_data$weekday)
rental_bikes_data$workingday=as.factor(rental_bikes_data$workingday)
rental_bikes_data$weathersit=as.factor(rental_bikes_data$weathersit)

#outlier analysis
numeric_index = sapply(rental_bikes_data,is.numeric)
View(numeric_index)
numeric_data = rental_bikes_data[,numeric_index]
cnames = colnames(numeric_data[,-1])
View(cnames)
for (i in 1:length(cnames))
   {
     assign(paste0("plot",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(rental_bikes_data))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="cnt")+
              ggtitle(paste("Box plot of cnt for",cnames[i])))
}
gridExtra::grid.arrange(plot1,plot2,plot3,plot4,ncol=4)
gridExtra::grid.arrange(plot5,ncol=1)
for(i in cnames){
    
     val = rental_bikes_data[,i][rental_bikes_data[,i] %in% boxplot.stats(rental_bikes_data[,i])$out]
     rental_bikes_data = rental_bikes_data[which(!rental_bikes_data[,i] %in% val),]
}
##bargraph
ggplot(rental_bikes_data, aes_string(x = rental_bikes_data$weekday,y=rental_bikes_data$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("mnth") + ylab('Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Rental Bike Analysis") +  theme(text=element_text(size=15))
####histogram
ggplot(rental_bikes_data,aes_string(x=rental_bikes_data$temp,y=rental_bikes_data$cnt))+
  geom_histogram(stat="bin",color='black')+geom_density()+theme_bw()+xlab("temp")+ylab("count")+ggtitle("Rental bikes analysis")+scale_x_continuous(breaks=pretty_breaks(n=10))+scale_y_continuous(breaks = pretty_breaks(n=10))

##correlation analysis
corrgram(rental_bikes_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#leaving atemp  column
rental_bikes_data<-rental_bikes_data[,-10]
#chi square test
factor_index = sapply(rental_bikes_data,is.factor)
factor_data = rental_bikes_data[,factor_index]
cnameschi = colnames(factor_data)
View(cnameschi)
for (i in 1:length(cnameschi))
{
  print(cnameschi[i])
  print(chisq.test(table(factor_data$workingday,factor_data[,i])),simulate.p.value = TRUE)
  
}
##droping holiday,season,instant


rental_bikes_data<-rental_bikes_data[,-5]
  View(rental_bikes_data)
  rental_bikes_data<-rental_bikes_data[,-2]
  rental_bikes_data<-rental_bikes_data[,-1]
  

##sampling so that we have train and test data
index = sample(1:nrow(rental_bikes_data), 0.8 * nrow(rental_bikes_data))
trainingdata=rental_bikes_data[index,]
View(trainingdata)
testdata=rental_bikes_data[-index,]
####decision tree  model
desicion_model = rpart(cnt ~ ., data = trainingdata, method = "anova")
View(desicion_model)
summary(desicion_model)
predicted = predict(desicion_model, testdata[,-9])
View(predicted)
MAPE = function(actual, predicted){
  mean(abs((actual - predicted)/actual))
}

MAPE(testdata[,9], predicted)
RMSE(testdata[,9],predicted)
#accuracy=80%
#rmse=890.69
##linear regression
library(usdm)
vifcor(rental_bikes_data[,-9],th=0.8)
linearmodel=lm(cnt~.,data=trainingdata)
linearpredict=predict(linearmodel,testdata[,-9])
summary(linearmodel)
imp_linear=as.data.frame(sort(confint(linearmodel)[,1],decreasing = TRUE))
names(imp_linear)="%imp variable"
MAPE(testdata[,9],linearpredict)
RMSE(testdata[,9],linearpredict)
View(imp_linear)
##accuracy 84%
##rmse=758.248
#####knn analysis
library(randomForest)
library(inTrees)
str(trainingdata)
rf <- randomForest(cnt ~., data = trainingdata, importance = TRUE, ntree=2000)
rf_predct=predict(rf,testdata[,-9])
MAPE(testdata[,9],rf_predct)
imp=as.data.frame(sort(importance(rf)[,1],decreasing=TRUE))
names(imp)<-"%mse"
imp
RMSE(testdata[,9],rf_predct)
summary(rf)
##accuracy=82%
##rmse=704.1732