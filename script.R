install.packages("caret")
install.packages("readxl")
install.packages("zoo")

library(zoo)
library("caret")
library('readxl')


#Turn the data into a DATAFRAME
GermanCredit <- read_xlsx("GermanCredit.xlsx", col_names=FALSE)
df<-data.frame(as.vector(strsplit(GermanCredit$X__2[1], ","))[[1]])
for (i in 2:13) {
  tmp<-as.vector(strsplit(GermanCredit$X__2[i], ","))[[1]]
  df<-cbind(df,tmp)
}
for (i in 1:13) {
  colnames(df)[i]<-GermanCredit$X__1[i]
}
df[apply(df, 2, function(x) x=="")] = NA
for (i in 1:13) {
  if(!is.na(as.numeric(as.character(df[1,i]))))
  {
    df[,i]<-as.numeric(as.character(df[,i]))
  }
}

#Function for Replacing NA
  myFun <- function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
      x
    } else {
      x[is.na(x)] <- names(which.max(table(x)))
      x
    }
  }



#Replacint NA and Dividing into Training and Testing Set
df2 <-as.data.frame(lapply(df, myFun))
inTrain<-createDataPartition(y=df$class,p=0.8 ,list=FALSE)
training<-df[inTrain,]
testing<-df[-inTrain,]




