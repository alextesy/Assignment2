
#Install Libraries
installLib<-function(){
  if("caret" %in% rownames(installed.packages())==FALSE)
    install.packages("caret")
  if("readxl" %in% rownames(installed.packages())==FALSE)
    install.packages("readxl")
  if("zoo" %in% rownames(installed.packages())==FALSE)
    install.packages("zoo")
  if("arules" %in% rownames(installed.packages())==FALSE)
    install.packages("arules")
  if("rpart" %in% rownames(installed.packages())==FALSE)
    install.packages("rpart")
  if("party" %in% rownames(installed.packages())==FALSE)
    install.packages("party")
  if("RColorBrewer" %in% rownames(installed.packages())==FALSE)
    install.packages("RColorBrewer")
  if("rattle" %in% rownames(installed.packages())==FALSE)
    install.packages("rattle")
}

#Load Libraries
lib<-function(){
  library("RColorBrewer")
  library("rattle")
  library("zoo")
  library("caret")
  library('readxl')
  library('arules')
  library('rpart')
  library('rpart.plot')
  library('party')
}

#Turn the data into a DATAFRAME
intitiate<-function(){
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
  return (df)
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




installLib()
lib()
df=intitiate()

df2 <-as.data.frame(lapply(df, myFun)) #Replacing NA 

#Discretization
df2$Average_Credit_Balance <- discretize(df2$Average_Credit_Balance,breaks = 4,method = 'frequency',labels = c("Low","Medium","High","Extreme"))
df2$over_draft <- discretize(df2$over_draft,breaks = 4,method = 'frequency',labels = c("Low","Medium","High","Extreme"))
df2$cc_age<- discretize(df2$cc_age,breaks = 3,method = 'frequency',labels = c("Young","Middle","Old"))



#Dividing into Training and Testing Set
inTrain<-createDataPartition(y=df2$class,p=0.8 ,list=FALSE)
training<-df2[inTrain,]
testing<-df2[-inTrain,]


#Gini Model
giniTree1=rpart(class~.,training,method = "class",control = list(minsplit=20))
fancyRpartPlot(giniTree1)


giniTree2=rpart(class~.,training,method = "class",control = list(minsplit=50))
fancyRpartPlot(giniTree2)


#Information Model
informationTree1=rpart(class~.,training,method = "class",parms = list(split = "information"),control = list(minsplit=20))
fancyRpartPlot(informationTree1)

informationTree2=rpart(class~.,training,method = "class",parms = list(split = "information"),control = list(minsplit=50))
fancyRpartPlot(informationTree2)


#Prediction
giniNbPredict1<-predict(giniTree1, newdata=testing)
giniNbPredict2<-predict(giniTree2, newdata=testing)
giniCM<-confusionMatrix(giniNbPredict1, reference=testing$class)
giniCM<-confusionMatrix(giniNbPredict2, reference=testing$class)

informationNbPredict<-predict(informationTree, newdata=testing)

giniCM<-confusionMatrix(giniNbPredict, reference=testing$class)
informationCM<-confusionMatrix(informationNbPredict, reference=testing$class)


