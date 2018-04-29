install.packages("caret")
install.packages("readxl")
install.packages("zoo")
install.packages("arules")
install.packages("rpart")


library("zoo")
library("caret")
library('readxl')
library('arules')
library('rpart')


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
inTrain<-createDataPartition(y=df2$class,p=0.8 ,list=FALSE)
training<-df2[inTrain,]x
testing<-df2[-inTrain,]



#Discretization
df2$Average_Credit_Balance <- discretize(df2$Average_Credit_Balance,breaks = 3,labels = c("Low","Medium","High"))
df2$over_draft <- discretize(df2$over_draft,breaks = 3,labels = c("Low","Medium","High"))
i=0
for(x in df2$cc_age){
  i=i+1
  if(is.na(x)==TRUE){
    next
  }else if(x<=30){
    df2[i,"cc_age"]="Young"
  } else if(x>30 & x<=60){
    df2[i,"cc_age"]="Middle"
  } else{
    df2[i,"cc_age"]="Old"
  }
}
df2$cc_age<-as.factor(df2$cc_age)

#MODEL
tree=rpart(class~.,training,method = "class",parms = list(split = "information"),control = list(minsplit=200)  )

