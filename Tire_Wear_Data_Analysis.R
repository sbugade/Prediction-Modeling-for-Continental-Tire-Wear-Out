
library(mice)
library(VIM)
library(dplyr)

tread_depth <- read.csv(file.choose(),header=TRUE,na.strings=c("",".","NA"))

summary(tread_depth)
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(tread_depth,2,p)


#Module 1 - Dataset Information
class(tread_depth)
dim(tread_depth)
names(tread_depth)
str(tread_depth)
head(tread_depth,n=15)
tail(tread_depth,n=15)

boxplot(tread_depth$td_mean,
        main = "td_mean distribution of data",
        xlab = "mean values",
        ylab = "td_mean",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)

boxplot(tread_depth,
        main = "All data columns",
        xlab = "Columns",
        ylab = "Values",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)

summary(tread_depth)

any(is.na(tread_depth))
sum(is.na(tread_depth))

# Convert categorical to numerical data 
tread_depth$rimtype<-as.numeric(tread_depth$rimtype)
tread_depth$tisize_radial<-as.numeric(tread_depth$tisize_radial)
tread_depth$wheelposition<-as.numeric(tread_depth$wheelposition)

# Handling blank values
levels <- levels(tread_depth$finding)
levels[length(levels) + 1] <- "UNK"
tread_depth$finding <- factor(tread_depth$finding, levels = levels)
tread_depth$finding[(tread_depth$finding ==" ")] <- "UNK"

levels <- levels(tread_depth$reason)
levels[length(levels) + 1] <- "UNKNOWN"
tread_depth$reason <- factor(tread_depth$reason, levels = levels)
tread_depth$reason[is.na(tread_depth$reason)] <- "UNKNOWN"

#2 %in% tread_depth$wheelposition
# Replacing NA values with imputation by calculating mean
tread_depth = transform(tread_depth, td_mean = ifelse(is.na(td_mean), mean(td_mean, na.rm=TRUE), td_mean))
tread_depth = transform(tread_depth, tacho = ifelse(is.na(tacho), mean(tacho, na.rm=TRUE), tacho))
tread_depth = transform(tread_depth, mileage = ifelse(is.na(mileage), mean(mileage, na.rm=TRUE), mileage))

any(is.na(tread_depth))
sum(is.na(tread_depth))

options(max.print=1000000)
md.pattern(tread_depth)
md.pairs(tread_depth)

summary(tread_depth)
library(bench)

boxplot(tread_depth$tacho,
        main = "tacho distribution of data",
        xlab = "mean values",
        ylab = "tacho",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)
boxplot(tread_depth$mileage,
        main = "mileage distribution of data",
        xlab = "mean values",
        ylab = "mileage",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)

summary(tread_depth)
boxplot(tread_depth$tacho)

tread_depth_new <- subset(tread_depth,tread_depth$td_mean < 13.558+1.5*(13.558-6.800) & tread_depth$td_mean > (6.800-1.5*(13.558-6.800)))
summary(tread_depth_new)
tread_depth_new<-  subset(tread_depth_new,tread_depth_new$mileage < 84235+1.5*(84235-6440) & tread_depth_new$mileage > (6440-1.5*(84235-6440)))
summary(tread_depth_new)
str(tread_depth_new1)
boxplot(tread_depth_new)

tread_depth_data<-  subset(tread_depth_new,tread_depth_new$tacho < 310401+1.5*(310401-115731) & tread_depth_new$tacho > (115731-1.5*(310401-115731)))
tread_depth_data<-  subset(tread_depth_data,tread_depth_data$mileage < 64908+1.5*(64908) & tread_depth_data$mileage > (-1.5*(64908)))
tread_depth_new6<-  subset(tread_depth_data,tread_depth_data$td_min < 12.780+1.5*(12.780-5.650) & tread_depth_data$td_min > (5.650-1.5*(12.780-5.650)))
#tread_depth_new4<-  subset(tread_depth_new4,tread_depth_new4$tisize_ratio < 80+1.5*(80-60) & tread_depth_new4$tisize_ratio > 60+1.5*(80-60) )

# Outliers for mileage
summary(tread_depth_data)
str(tread_depth_new6)
boxplot(tread_depth_new6,
        main = "All data columns",
        xlab = "Columns",
        ylab = "Values",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE
)


set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(tread_depth_new6), size = floor(.75*nrow(tread_depth_new6)), replace = F)
train <- tread_depth_new6[sample, ]
test  <- tread_depth_new6[-sample, ]

str(test)

output=lm(td_mean~mileage+rimtype+tacho+wheelposition+tisize_rim+tisize_radial+tisize_width+tisize_ratio+td_min+numspikes_na,data=train)
summary(output)

library(olsrr)

k <- ols_step_all_possible(output)
plot(k)

tmp_test<-
  library(lubridate)
a = mdy(test$date)
year(a)

plot(a,train$td_mean)           # Date ~ td_means 

library(car)
vif(output)

scatter.smooth(x=test$td_mean, y=test$mileage, main="Dist ~ Speed")  # scatterplot

#y = 1.24+0.02*test$wheelposition+(-1.11*test$tisize_radial)+0.01*test$tisize_width+0.02*test$tisize_ratio+0.83*test$td_min+0.02*test$numspikes_na

mean_square<-mean(train$y^2)
vif(y)
mean_square


boxplot(train$td_mean)
library(rpart)
plot(test$date,test$td_mean,ylab="test$td_mean")
cor(tread_depth_new6)

tmp_test<-
  library(lubridate)
a = mdy(test$date)
year(a)

ggplot(data = test, mapping = aes(x = a,y=td_mean)) +
  geom_line()

hist(test$td_mean) 
x <- train$td_mean
h<-hist(x, breaks=10, col="red", xlab="td_mean", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

normFunc <- function(train){(train-mean(train, na.rm = T))/sd(train, na.rm = T)}
normFunc
