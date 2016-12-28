# Download the data files and put them in the same directory
# if your are using Rstudio, you can use the Session button from the main menu
# to set the working directory 
# Make sure you have the following libraries installed for the analysis:
#  lubridate
#  ggplot2
#  grid
#  gridExtra
#  scales
#  Hmisc
#  corrplot
#  randomForest
#  rattle
#  caret
#  e1071
#  RGTk2 (for rattle)
#           
# Since the training of the models is computationally intensive, it is suggested 
#to save
# all the R objects created in the session by running the command:
# save.image("yourfilenamechoice.Rdata")
# Be ware that you save the objects after loading the Rdata, else you can loose 
# Previously saved data. 

#setwd("//umons.ac.be/users/531323/Desktop/Occupancy/data_paper/")
setwd("D:/Aakash_Documents/MS_Collections/AcceptanceFromSaintPeters/ClassStuff/DS630_MachineLearning/FInalProjectWork/Occupancy-detection-data-master")

# Run the save and load commands according to your training progress. 
# Pay attention to not
# overwrite the file with empty or less data
save.image(file="occupancy_last.RData")

#load("occupancy_last.RData")


# Loading data
datatraining <- read.table("datatraining.txt",header=TRUE,sep=",")
datatesting <- read.table("datatest.txt",header=TRUE,sep=",")
datatesting2 <- read.table("datatest2.txt",header=TRUE,sep=",")

#Reviewing the data classes
str(datatraining)
str(datatesting)
str(datatesting2)

datatraining$Occupancy <- as.factor(datatraining$Occupancy)
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting2$Occupancy  <- as.factor(datatesting2$Occupancy)

# Formating the date class for all the files
datatraining$date <- as.POSIXct(datatraining$date,tz="UTC") 
datatesting$date <- as.POSIXct(datatesting$date,tz="UTC") 
datatesting2$date <- as.POSIXct(datatesting2$date,tz="UTC") 


weekend_weekday <- function(x) {
        val <- weekdays(x)
        if (val == "Saturday" | val == "Sunday") {
                val2 = "Weekend"
        }
        else {
                val2= "Weekday"
        }
        return(val2)
}

# Used for plotting
Relevel_weekend <- function(x) {
        
        if (x == "Weekend") {
                val2 = 0
        }
        else {
                val2= 1
        }
        return(val2)
}



# Used to add the seconds and weekend weekday  columns
datatraining_b <- datatraining
# Use to add the weekend/weekday label
datatraining_b$WeekStatus <-unlist(lapply(datatraining$date,weekend_weekday))
summary(datatraining_b)
str(datatraining_b)
datatraining_b$WeekStatus <-as.factor(datatraining_b$WeekStatus)
str(datatraining_b)

# for datatesting
datatesting_b <- datatesting


# to add the weekend/weekday label
datatesting_b$WeekStatus <-unlist(lapply(datatesting_b$date,weekend_weekday))
summary(datatesting_b)
str(datatesting_b)
datatesting_b$WeekStatus <-as.factor(datatesting_b$WeekStatus)

# for datatesting 2
datatesting2_b <- datatesting2

# to add the weekend/weekday label
datatesting2_b$WeekStatus <-unlist(lapply(datatesting2_b$date,weekend_weekday))
summary(datatesting2_b)
str(datatesting2_b)
datatesting2_b$WeekStatus <-as.factor(datatesting2_b$WeekStatus)
str(datatesting2_b)

# Turning the Occupancy variable into a factor
datatraining$Occupancy <- as.factor(datatraining$Occupancy)
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting2$Occupancy  <- as.factor(datatesting2$Occupancy)

# Reviewing the data types again
str(datatraining)
str(datatraining_b)
str(datatesting)
str(datatesting2)

# Examining the classes distribution for skewness
prop.table(table(datatraining$Occupancy))
# 0              1
# 0.7876704      0.21232
prop.table(table(datatesting$Occupancy))
# 0              1
#0.635272 0.364728 
prop.table(table(datatesting2$Occupancy))
# 0         1 
# 0.7898893 0.2101107 


# Checking that there are not NAs in the data sets... neccesary for model training.
summary(datatraining)
summary(datatesting)
summary(datatesting2)

# Exploratory Figure
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
pushViewport(viewport(layout = grid.layout(6, 1)))

myplot1 <- ggplot(datatesting,aes(date))+geom_line(color="Red",aes(y=Temperature))+ylab("Temperature")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot2 <- ggplot(datatesting,aes(date))+geom_line(color="Blue",aes(y=Humidity))+ylab("Humidity")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot3 <- ggplot(datatesting,aes(date))+geom_line(color="deepskyblue1",aes(y=HumidityRatio))+ylab("HumidityRatio")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot4 <- ggplot(datatesting,aes(date))+geom_line(color="Green",aes(y=CO2))+ylab("CO2 (ppm)")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot5 <- ggplot(datatesting,aes(date))+geom_line(color="gold4",aes(y=Light))+ylab("Light (Lux)")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))        

myplot6 <- ggplot(datatesting,aes(date))+geom_line(color="Black",aes(y=as.numeric(Occupancy)))+ylab("Occupancy")+xlab("Time")+
        scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                         limits=as.POSIXct(c("2015-02-03 8:00","2015-02-04 9:00"),tz="GMT"))+
        theme(axis.text.x=element_text(angle=90,hjust=1))  

#ggplot(dataaggregated3bclenaed,aes(date))+geom_line(color="Orange",aes(y=HumidityRatio))+ylab("HumidityRatio (kg/kg)")+xlab("Time")+
#       scale_x_datetime(breaks=date_breaks("30 min"),labels=date_format("%H:%M"),
#                        limits=as.POSIXct(c("2015-02-10 11:50","2015-02-10 18:50"),tz="GMT"))+theme(axis.text.x=element_text(angle=90,hjust=1))
#



print(myplot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(myplot2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(myplot3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(myplot4, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(myplot5, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(myplot6, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))

# These commands are used to ensure that the time axis for the 
# plots are all aligned 

myplot1 <- ggplot_gtable(ggplot_build(myplot1))
myplot2 <- ggplot_gtable(ggplot_build(myplot2))
myplot3 <- ggplot_gtable(ggplot_build(myplot3))
myplot4 <- ggplot_gtable(ggplot_build(myplot4))
myplot5 <- ggplot_gtable(ggplot_build(myplot5))
myplot6 <- ggplot_gtable(ggplot_build(myplot6))


maxWidth = unit.pmax(myplot1$widths[2:3],myplot2$widths[2:3],myplot3$widths[2:3],
                     myplot4$widths[2:3],myplot5$widths[2:3],myplot6$widths[2:3])
myplot1$widths[2:3] <- maxWidth
myplot2$widths[2:3] <- maxWidth
myplot3$widths[2:3] <- maxWidth
myplot4$widths[2:3] <- maxWidth
myplot5$widths[2:3] <- maxWidth
myplot6$widths[2:3] <- maxWidth

grid.arrange(myplot1, myplot2,
             myplot3, myplot4,
             myplot5, myplot6,ncol=1)
# Run the commented line if you want to safe the plot in a png file
# png(file="occupancy2.png",width=1250,height=1200,res=75)
grid.arrange(myplot1, myplot2,
             myplot3, myplot4,
             myplot5, myplot6,ncol=1)


ModelLDA <- train(Occupancy~.-date,method="lda",data=datatraining)
ModelLDA
plot(varImp(ModelLDA,scale=TRUE))
# Pairs plot
dataset = data.frame(occupancystatus = datatraining[,"Occupancy"],
                                              lda=predict(ModelLDA,datatraining),
                                              temperature=datatraining[,"Temperature"],
                                              Light=datatraining[,"Light"])
                         
plda <- ggplot(dataset) + geom_point(aes(temperature,Light, colour = occupancystatus, shape = occupancystatus), size = 2.5) 
                         
                         
plot(plda)

                         
cols2 <- character(nrow(datatraining))
cols2[] <- "black"
cols2[datatraining$Occupancy %in% c("0")] <- "green"
cols2[datatraining$Occupancy %in% c("1")] <- "blue"
                         
pairs(datatraining[2:6], col=cols2, cex=1.1, cex.labels=1.5)


# weekend_weekday
cols2 <- character(nrow(datatraining_b))
cols2[] <- "black"
cols2[datatraining_b$Occupancy %in% c("0")] <- "green"
cols2[datatraining_b$Occupancy %in% c("1")] <- "blue"

#pairs(datatraining_b[c(2,3,4,5,6,8,9)], col=cols2, cex=1.1, cex.labels=1.5)

pairs(datatraining_b[c(2,3,4,5,6,8)], col=cols2, cex=1.1, cex.labels=1.5)

#releveling the weekend_weekday for visualization purposes


datatraining_b_2 <-datatraining_b

datatraining_b_2$WeekStatus <- unlist(lapply(datatraining_b_2$WeekStatus,Relevel_weekend))



pairs(datatraining_b_2[c(2,3,4,5,6,8)], col=cols2, cex=1.1, cex.labels=1.5)


# Obtaining correlation between variables

#cor(datatraining[2:6])
cor(datatraining_b_2[c(2,3,4,5,6,8)])


library(Hmisc)
correlation_result<-rcorr(as.matrix(datatraining[2:6]))
correlation_result
correlation_result$P


rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]),type = "pearson")
rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]),type = "spearman")



correlation_result_b<-rcorr(as.matrix(datatraining_b_2[c(2,3,4,5,6,8,9)]))
correlation_result_b
correlation_result_b$P

# Plotting the correlation plot

library(corrplot)
corrplot(correlation_result_b$r,type="upper", order="hclust", tl.col="black", tl.srt=45)

# Loading caret, randomForest, svm libraries

library(randomForest)
library(rattle)
library(MASS)
library(ISLR)
#
# When training the models, also execute the set.seed command to ensure 
# the tranined model is reproducible
# The models with ***_b include training data with NS and WS
# 

set.seed(1234)
ModelRF_ALL_b <- train(Occupancy~.-date,method="rf",data=datatraining_b)
ModelRF_ALL_b
# Accuracy 0.993
plot(ModelRF_ALL_b)
ModelRF_ALL_b$finalModel
varImp(ModelRF_ALL_b)
plot(varImp(ModelRF_ALL_b,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b,datatraining_b))
#100.0

sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b,datatesting_b))/dim(datatesting_b)[1]*100
#  95.53% acuracy 
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b,datatesting2_b))/dim(datatesting2_b)[1]*100
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b,datatesting_b))
# 0.9553 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b,datatesting2_b))
# 0.9806 Acuracy
ModelRF_ALL_b$finalModel



# Creating a plot of the random forest error
dev.off()
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,1], 
     ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1], ModelRF_ALL_b$finalModel$err.rate[,2],
                  ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',col='blue',axes=TRUE,xlab="Number of Trees",ylab="Error")
par(new=TRUE)
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,2],
     ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1], ModelRF_ALL_b$finalModel$err.rate[,2],
                  ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',col="red",axes=TRUE,xlab="Number of Trees",ylab="Error")
par(new=TRUE)
plot(1:500,ModelRF_ALL_b$finalModel$err.rate[,3],ylim=range(c(ModelRF_ALL_b$finalModel$err.rate[,1],
                                                              ModelRF_ALL_b$finalModel$err.rate[,2],
                                                              ModelRF_ALL_b$finalModel$err.rate[,3])),type='l',xlab="Number of Trees",ylab="Error")

legend("topright",legend= c("Not Occupied (0)","Occupied (1)"), bty ="n", lwd=1,
       col=c("blue","red","black"))
##







# T, H, Light, W, NS, WS (no CO2, no Light)

set.seed(1234)
ModelRF_ALL_b_no_CO2noLight <- train(Occupancy~.-date-CO2-Light,method="rf",data=datatraining_b)
ModelRF_ALL_b_no_CO2noLight
# Accuracy 0.9924
plot(ModelRF_ALL_b_no_CO2noLight)
ModelRF_ALL_b_no_CO2noLight$finalModel
varImp(ModelRF_ALL_b_no_CO2noLight)
plot(varImp(ModelRF_ALL_b_no_CO2noLight,scale=TRUE))


set.seed(1234)
confusionMatrix(datatraining_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatraining_b))
#100.0
set.seed(1234)
sum(datatesting_b$Occupancy==predict(ModelRF_ALL_b_no_CO2noLight,datatesting_b))/dim(datatesting_b)[1]*100
#  94.86% acuracy 
set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatesting_b))
# 94.86 Acuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelRF_ALL_b_no_CO2noLight,datatesting2_b))
# 91.66 Acuracy


# Creating a plot of the random forest error
dev.off()

par(new=TRUE)
plot(1:500,ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,2],
     ylim=range(c(ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,1], ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,2],
                  ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,3])),type='l',col="red",axes=TRUE,xlab="Number of Trees",ylab="Error")
par(new=TRUE)
plot(1:500,ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,3],ylim=range(c(ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,1],
                                                                            ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,2],
                                                                            ModelRF_ALL_b_no_CO2noLight$finalModel$err.rate[,3])),type='l',xlab="Number of Trees",ylab="Error")
legend("topright",legend= c("Not Occupied (0)","Occupied (1)"), bty ="n", lwd=1,
       col=c("red","black"))



## lda models 
ModelLDA_ALL_b <- train(Occupancy~.-date,method="lda",data=datatraining_b)

ModelLDA_ALL_b$finalModel$xNames

ModelLDA_ALL_b$finalModel 
ModelLDA_ALL_b

varImp(ModelLDA_ALL_b)
plot(varImp(ModelLDA_ALL_b,scale=TRUE))

#0.9878 
confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_ALL_b,datatraining_b))
#0.9885


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_ALL_b,datatesting_b))
# 0.979 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_ALL_b,datatesting2_b))
# 0.9933 Accuracy  
ModelLDA_ALL_b$finalModel


### no CO2 no Light, with WS, NS
ModelLDA_noCO2NoLight_b <- train(Occupancy~.-date-CO2-Light,method="lda",data=datatraining_b)
ModelLDA_noCO2NoLight_b$finalModel$xNames
ModelLDA_noCO2NoLight_b$finalModel
# 0.8571

confusionMatrix(datatraining_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatraining_b))
#0.8578


set.seed(1234)
confusionMatrix(datatesting_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatesting_b))
# 0.8593 Accuracy
set.seed(1234)
confusionMatrix(datatesting2_b$Occupancy,predict(ModelLDA_noCO2NoLight_b,datatesting2_b))
# 0.867 Accuracy
ModelLDA_noCO2NoLight_b
