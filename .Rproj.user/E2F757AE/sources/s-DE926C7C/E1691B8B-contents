install.packages("caret")
install.packages("readxl")

library("caret")
library('readxl')
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
inTrain<-createDataPartition(y=df$class,p=0.8 ,list=FALSE)
training<-df[inTrain,]
testing<-df[-inTrain,]
index=c(1,2,5,6,9,12)
for(i in length(index))
  df[,index[i]]=as.numeric(as.character(df[,index[i]]))