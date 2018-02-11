setwd('C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/Greak Lakes Hackathon - Beyond Infinity')

#Load the raw data
glbi_train_raw=read.csv('train_4aqQp50.csv', header = T)
glbi_test_raw=read.csv('test_VJP2kVH.csv', header = T)

#Quick load the preprocessed files (this was used after the preprocessing was done!)
glbi_train= read.csv('glbi_traind3.csv')
glbi_test= read.csv('glbi_testd.csv')

#Check for NA & convert Blank values to NA
glbi_train_raw[glbi_train_raw==""]= NA
glbi_test_raw[glbi_test_raw==""]= NA
colSums(is.na(glbi_train_raw))
colSums(is.na(glbi_test_raw))

str(glbi_train_raw)

plot(density(glbi_train_raw$QS_OVERALL, na.rm = T))

#Remove the PRT_ID column
glbi_train_raw=glbi_train_raw[-1]
glbi_test_raw=glbi_test_raw[-1]

#Convert Date Sale columns to Date type
library(lubridate)
glbi_train_raw$DATE_SALE= dmy(glbi_train_raw$DATE_SALE)
glbi_test_raw$DATE_SALE= dmy(glbi_test_raw$DATE_SALE)

#Convert Date Build columns to Date type
glbi_train_raw$DATE_BUILD= dmy(glbi_train_raw$DATE_BUILD)
glbi_test_raw$DATE_BUILD= dmy(as.character(glbi_test_raw$DATE_BUILD))

#Convert Bedroom into factor
glbi_train_raw$N_BEDROOM= as.factor(as.character(glbi_train_raw$N_BEDROOM))
glbi_test_raw$N_BEDROOM= as.factor(as.character(glbi_test_raw$N_BEDROOM))

#Convert Bathroom into factor
glbi_train_raw$N_BATHROOM= as.factor(as.character(glbi_train_raw$N_BATHROOM))
glbi_test_raw$N_BATHROOM= as.factor(as.character(glbi_test_raw$N_BATHROOM))

#Convert Room into factor
glbi_train_raw$N_ROOM= as.factor(as.character(glbi_train_raw$N_ROOM))
glbi_test_raw$N_ROOM= as.factor(as.character(glbi_test_raw$N_ROOM))

glbi_train= glbi_train_raw
glbi_test= glbi_test_raw

##Rectify the Factor Levels in various columns 
#AREA column - Train set
levels(glbi_train$AREA)
n1=nrow(glbi_train)
for(i in 1:n1){
  if(glbi_train$AREA[i]=='Ana Nagar'|| glbi_train$AREA[i]=='Ann Nagar'){
    glbi_train$AREA[i]='Anna Nagar'
  }
  if(glbi_train$AREA[i]=='Adyr'){
    glbi_train$AREA[i]='Adyar'
  }
  else if(glbi_train$AREA[i]=='Chormpet'|| glbi_train$AREA[i]=='Chrmpet'|| glbi_train$AREA[i]=='Chrompt'){
    glbi_train$AREA[i]='Chrompet'
  }
  else if(glbi_train$AREA[i]=='Karapakam'){
    glbi_train$AREA[i]='Karapakkam'
  }
  else if(glbi_train$AREA[i]=='KKNagar'){
    glbi_train$AREA[i]='KK Nagar'
  }
  else if(glbi_train$AREA[i]=='TNagar'){
    glbi_train$AREA[i]='T Nagar'
  }
  else if(glbi_train$AREA[i]=='Velchery'){
    glbi_train$AREA[i]='Velachery'
  }
}
table(glbi_train$AREA)
glbi_train$AREA=as.factor(as.character(glbi_train$AREA))

#AREA column - Test set
levels(glbi_test$AREA)
n2=nrow(glbi_test)
for(i in 1:n2){
  if(glbi_test$AREA[i]=='Adyr'){
    glbi_test$AREA[i]='Adyar'
  }
  else if(glbi_test$AREA[i]=='Ana Nagar'|| glbi_test$AREA[i]=='Ann Nagar'){
    glbi_test$AREA[i]='Anna Nagar'
  }
  else if(glbi_test$AREA[i]=='Chormpet'|| glbi_test$AREA[i]=='Chrmpet'|| glbi_test$AREA[i]=='Chrompt'){
    glbi_test$AREA[i]='Chrompet'
  }
  else if(glbi_test$AREA[i]=='Karapakam'){
    glbi_test$AREA[i]='Karapakkam'
  }
  else if(glbi_test$AREA[i]=='KKNagar'){
    glbi_test$AREA[i]='KK Nagar'
  }
  else if(glbi_test$AREA[i]=='TNagar'){
    glbi_test$AREA[i]='T Nagar'
  }
  else if(glbi_test$AREA[i]=='Velchery'){
    glbi_test$AREA[i]='Velachery'
  }
}
table(glbi_test$AREA)
glbi_test$AREA=as.factor(as.character(glbi_test$AREA))
#glbi_test=glbi_test[-19]

#Sale_Cond column - Train set
levels(glbi_train$SALE_COND)
for(i in 1:n1){
  if(glbi_train$SALE_COND[i]=='Ab Normal'){
    glbi_train$SALE_COND[i]='AbNormal'
  }
  else if(glbi_train$SALE_COND[i]=='Adj Land'){
    glbi_train$SALE_COND[i]='AdjLand'
  }
  else if(glbi_train$SALE_COND[i]=='Partiall' || glbi_train$SALE_COND[i]=='PartiaLl' ){
    glbi_train$SALE_COND[i]='Partial'
  }
}
table(glbi_train$SALE_COND)
glbi_train$SALE_COND=as.factor(as.character(glbi_train$SALE_COND))

#Sale_Cond column - Train set
levels(glbi_test$SALE_COND)
for(i in 1:n2){
  if(glbi_test$SALE_COND[i]=='Ab Normal'){
    glbi_test$SALE_COND[i]='AbNormal'
  }
  else if(glbi_test$SALE_COND[i]=='Adj Land'){
    glbi_test$SALE_COND[i]='AdjLand'
  }
  else if(glbi_test$SALE_COND[i]=='Partiall' || glbi_test$SALE_COND[i]=='PartiaLl' ){
    glbi_test$SALE_COND[i]='Partial'
  }
}
table(glbi_test$SALE_COND)
glbi_test$SALE_COND=as.factor(as.character(glbi_test$SALE_COND))

#Park_Facil column - Train set
levels(glbi_train$PARK_FACIL)
for(i in 1:n1){
  if(glbi_train$PARK_FACIL[i]=='Noo'){
    glbi_train$PARK_FACIL[i]='No'
  }
}
table(glbi_train$PARK_FACIL)
glbi_train$PARK_FACIL=as.factor(as.character(glbi_train$PARK_FACIL))

#Park_Facil column - Test set
levels(glbi_test$PARK_FACIL)
for(i in 1:n2){
  if(glbi_test$PARK_FACIL[i]=='Noo'){
    glbi_test$PARK_FACIL[i]='No'
  }
}
table(glbi_test$PARK_FACIL)
glbi_test$PARK_FACIL=as.factor(as.character(glbi_test$PARK_FACIL))

#Buildtype column - Train set
levels(glbi_train$BUILDTYPE)
for(i in 1:n1){
  if(glbi_train$BUILDTYPE[i]=='Comercial' || glbi_train$BUILDTYPE[i]=='Commercil'){
    glbi_train$BUILDTYPE[i]='Commercial'
  }
  else if(glbi_train$BUILDTYPE[i]=='Other'){
    glbi_train$BUILDTYPE[i]='Others'
  }
}
table(glbi_train$BUILDTYPE)
glbi_train$BUILDTYPE=as.factor(as.character(glbi_train$BUILDTYPE))

#Buildtype column - Test set
levels(glbi_test$BUILDTYPE)
for(i in 1:n2){
  if(glbi_test$BUILDTYPE[i]=='Comercial' || glbi_test$BUILDTYPE[i]=='Commercil'){
    glbi_test$BUILDTYPE[i]='Commercial'
  }
  else if(glbi_test$BUILDTYPE[i]=='Other'){
    glbi_test$BUILDTYPE[i]='Others'
  }
}
table(glbi_test$BUILDTYPE)
glbi_test$BUILDTYPE=as.factor(as.character(glbi_test$BUILDTYPE))

#Utility_avail column - Train set
levels(glbi_train$UTILITY_AVAIL)
for(i in 1:n1){
  if(glbi_train$UTILITY_AVAIL[i]=='All Pub'){
    glbi_train$UTILITY_AVAIL[i]='AllPub'
  }
}
table(glbi_train$UTILITY_AVAIL)
glbi_train$UTILITY_AVAIL=as.factor(as.character(glbi_train$UTILITY_AVAIL))

#Utility_avail column - Test set
levels(glbi_test$UTILITY_AVAIL)
for(i in 1:n2){
  if(glbi_test$UTILITY_AVAIL[i]=='All Pub'){
    glbi_test$UTILITY_AVAIL[i]='AllPub'
  }
}
table(glbi_test$UTILITY_AVAIL)
glbi_test$UTILITY_AVAIL=as.factor(as.character(glbi_test$UTILITY_AVAIL))

#Street column - Train set
levels(glbi_train$STREET)
for(i in 1:n1){
  if(glbi_train$STREET[i]=='NoAccess'){
    glbi_train$STREET[i]='No Access'
  }
  else if(glbi_train$STREET[i]=='Pavd'){
    glbi_train$STREET[i]='Paved'
  }
  
}
table(glbi_train$STREET)
glbi_train$STREET=as.factor(as.character(glbi_train$STREET))

#Street column - Train set
levels(glbi_test$STREET)
for(i in 1:n2){
  if(glbi_test$STREET[i]=='NoAccess'){
    glbi_test$STREET[i]='No Access'
  }
  else if(glbi_test$STREET[i]=='Pavd'){
    glbi_test$STREET[i]='Paved'
  }
  
}
table(glbi_test$STREET)
glbi_test$STREET=as.factor(as.character(glbi_test$STREET))

#Combine test and train set using dplyr
library(dplyr)
glbi_total= bind_rows(glbi_train[,-c(21)],glbi_test)
glbi_total= as.data.frame(glbi_total)

#Convert char type variables to factor
glbi_total$AREA= as.factor(glbi_total$AREA)
glbi_total$SALE_COND= as.factor(glbi_total$SALE_COND)
glbi_total$BUILDTYPE= as.factor(glbi_total$BUILDTYPE)
glbi_total$STREET= as.factor(glbi_total$STREET)

#Calculate RMSE
Smy_RMSE= function(x) {
  sum1=0
  n=NROW(x)
  for(i in 1:n){
    sum1= sum1+ (x[i]-validation$SALES_PRICE[i])^2
  }
  rmse=sqrt(sum1/n)
  return(rmse)
}

#Create output xls file
output_xls= function(x){
  SALES_PRICE=x
  PRT_ID= glbi_test_raw$PRT_ID
  av_glbi=cbind.data.frame(PRT_ID,SALES_PRICE)
  write.csv(av_glbi,'C:/Users/SUBHRANIL ROY/Desktop/av_glbi.csv',row.names = FALSE)
}

#Convert the data into dummy/numeric set
library(caret)
dummy1= dummyVars("~.",data = glbi_train)
glbi_traind= data.frame(predict(dummy1, newdata=glbi_train))
dim(glbi_traind)

dummy1= dummyVars("~.",data = glbi_test)
glbi_testd= data.frame(predict(dummy1, newdata=glbi_test))
dim(glbi_testd)
#glbi_test$DATE_SALE= dmy(glbi_test_raw$DATE_SALE)

#Use missforest to replace outlet size missing values
library(missForest)
x4<-missForest(glbi_traind[-52])
# impute missing value
glbi_traind<-x4$ximp
colSums(is.na(glbi_traind))
glbi_traind$SALES_PRICE= glbi_train_raw$SALES_PRICE

x4<-missForest(glbi_testd)
glbi_testd<-x4$ximp
colSums(is.na(glbi_testd))

#Run LOF for multivariate outlier detection
library(Rlof)
glbi_traind_lof= lof(glbi_traind,5)
lof=glbi_traind_lof
glbi_traind=cbind(glbi_traind,lof)
View(glbi_traind)
plot(density(glbi_traind$glbi_traind_lof))
NROW(which(glbi_traind$glbi_traind_lof>1.5))

glbi_testd_lof= lof(glbi_testd,5)
lof=glbi_testd_lof
glbi_testd=cbind(glbi_testd,lof)
View(glbi_testd)
plot(density(glbi_testd$glbi_testd_lof))
NROW(which(glbi_testd$glbi_testd_lof>1.5))

#Further investigate the values which appear to be multivariate outliers
head(glbi_traind[which(glbi_traind$glbi_traind_lof>1.5),])
head(glbi_testd[which(glbi_testd$glbi_testd_lof>1.5),])

#Save the preprocessed train and test in xls
write.csv(glbi_testd,'C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/Greak Lakes Hackathon - Beyond Infinity/glbi_testd.csv',row.names = F)

#Add interaction variable
#Registration and Commission interaction
glbi_traind$RegComm= glbi_traind$REG_FEE*glbi_traind$COMMIS
glbi_testd$RegComm= glbi_testd$REG_FEE*glbi_testd$COMMIS

#Registration and Commission interaction polynomial
glbi_traind$RegCommPol= glbi_traind$RegComm^2
glbi_testd$RegCommPol= glbi_testd$RegComm^2

#Registration polynomial term
glbi_traind$RegPol= glbi_traind$REG_FEE^2
glbi_testd$RegPol= glbi_testd$REG_FEE^2

#Registration polynomial term 2
glbi_traind$RegPol2= glbi_traind$REG_FEE^3
glbi_testd$RegPol2= glbi_testd$REG_FEE^3

#Registration polynomial term 3
glbi_traind$RegPol3= glbi_traind$REG_FEE^4
glbi_testd$RegPol3= glbi_testd$REG_FEE^4

#Commission polynomial term
glbi_traind$ComPol= glbi_traind$COMMIS^2
glbi_testd$ComPol= glbi_testd$COMMIS^2

#Commission polynomial term 2
glbi_traind$ComPol2= glbi_traind$COMMIS^3
glbi_testd$ComPol2= glbi_testd$COMMIS^3

#Registration and Sqft interaction
glbi_traind$RegSqft= glbi_traind$REG_FEE*glbi_traind$INT_SQFT
glbi_testd$RegSqft= glbi_testd$REG_FEE*glbi_testd$INT_SQFT

#RegPol and Sqft interaction
glbi_traind$RegPolSqft= glbi_traind$RegPol*glbi_traind$INT_SQFT
glbi_testd$RegPolSqft= glbi_testd$RegPol*glbi_testd$INT_SQFT

#CommPol and Sqft interaction
glbi_traind$CommPolSqft= glbi_traind$ComPol*glbi_traind$INT_SQFT
glbi_testd$CommPolSqft= glbi_testd$ComPol*glbi_testd$INT_SQFT

#Square ft polynomial term
glbi_traind$SqftPol= glbi_traind$INT_SQFT^2
glbi_testd$SqftPol= glbi_testd$INT_SQFT^2

#Square ft polynomial term 2
glbi_traind$SqftPol2= glbi_traind$INT_SQFT^3
glbi_testd$SqftPol2= glbi_testd$INT_SQFT^3

#Square ft polynomial term 3
glbi_traind$SqftPol3= glbi_traind$INT_SQFT^4
glbi_testd$SqftPol3= glbi_testd$INT_SQFT^4
#------------------------------------------------------------------------#
#Square ft polynomial and Regristration interaction
glbi_traind$RegSqftPol= glbi_traind$SqftPol*glbi_traind$REG_FEE
glbi_testd$RegSqftPol= glbi_testd$SqftPol*glbi_testd$REG_FEE

#Square ft polynomial2 and Regristration interaction
glbi_traind$RegSqftPol2= glbi_traind$SqftPol2*glbi_traind$REG_FEE
glbi_testd$RegSqftPol2= glbi_testd$SqftPol2*glbi_testd$REG_FEE

#Square ft polynomial3 and Regristration interaction
glbi_traind$RegSqftPol3= glbi_traind$SqftPol3*glbi_traind$REG_FEE
glbi_testd$RegSqftPol3= glbi_testd$SqftPol3*glbi_testd$REG_FEE

#Square ft polynomial and Commission interaction
glbi_traind$ComSqftPol= glbi_traind$SqftPol*glbi_traind$COMMIS
glbi_testd$ComSqftPol= glbi_testd$SqftPol*glbi_testd$COMMIS

#Square ft polynomial2 and Commission interaction
glbi_traind$ComSqftPol2= glbi_traind$SqftPol2*glbi_traind$COMMIS
glbi_testd$ComSqftPol2= glbi_testd$SqftPol2*glbi_testd$COMMIS

#Square ft polynomial3 and Commission interaction
glbi_traind$ComSqftPol3= glbi_traind$SqftPol3*glbi_traind$COMMIS
glbi_testd$ComSqftPol3= glbi_testd$SqftPol3*glbi_testd$COMMIS

#Square ft and RegComm interaction
glbi_traind$RegComSqft= glbi_traind$INT_SQFT*glbi_traind$RegComm
glbi_testd$RegComSqft= glbi_testd$INT_SQFT*glbi_testd$RegComm

#Square ft pol and RegComm interaction
glbi_traind$RegComSqftPol= glbi_traind$SqftPol*glbi_traind$RegComm
glbi_testd$RegComSqftPol= glbi_testd$SqftPol*glbi_testd$RegComm

#Square ft pol2 and RegComm interaction
glbi_traind$RegComSqftPol2= glbi_traind$SqftPol2*glbi_traind$RegComm
glbi_testd$RegComSqftPol2= glbi_testd$SqftPol2*glbi_testd$RegComm

#Square ft pol3 and RegComm interaction
glbi_traind$RegComSqftPol3= glbi_traind$SqftPol3*glbi_traind$RegComm
glbi_testd$RegComSqftPol3= glbi_testd$SqftPol3*glbi_testd$RegComm
#----------------------------------------------------------------------#
##Registration Interaction
#Area.Adyar and Registration interaction
glbi_traind$RegAr.ad= glbi_traind$AREA.Adyar*glbi_traind$REG_FEE
glbi_testd$RegAr.ad= glbi_testd$AREA.Adyar*glbi_testd$REG_FEE

#Area.ANagar and Registration interaction
glbi_traind$RegAr.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$REG_FEE
glbi_testd$RegAr.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$REG_FEE

#Area.Chrompet and Registration interaction
glbi_traind$RegAr.ch= glbi_traind$AREA.Chrompet*glbi_traind$REG_FEE
glbi_testd$RegAr.ch= glbi_testd$AREA.Chrompet*glbi_testd$REG_FEE

#Area.Karapakkam and Registration interaction
glbi_traind$RegAr.kp= glbi_traind$AREA.Karapakkam*glbi_traind$REG_FEE
glbi_testd$RegAr.kp= glbi_testd$AREA.Karapakkam*glbi_testd$REG_FEE

#Area.KKnagar and Registration interaction
glbi_traind$RegAr.kkn= glbi_traind$AREA.KK.Nagar*glbi_traind$REG_FEE
glbi_testd$RegAr.kkn= glbi_testd$AREA.KK.Nagar*glbi_testd$REG_FEE

#Area.Tnagar and Registration interaction
glbi_traind$RegAr.tn= glbi_traind$AREA.T.Nagar*glbi_traind$REG_FEE
glbi_testd$RegAr.tn= glbi_testd$AREA.T.Nagar*glbi_testd$REG_FEE

#Bathroom.1 and Registration interaction
glbi_traind$Regbth1= glbi_traind$N_BATHROOM.1*glbi_traind$REG_FEE
glbi_testd$Regbth1= glbi_testd$N_BATHROOM.1*glbi_testd$REG_FEE

#Sale condition abnormal and Registration interaction
glbi_traind$RegscAbn= glbi_traind$SALE_COND.AbNormal*glbi_traind$REG_FEE
glbi_testd$RegscAbn= glbi_testd$SALE_COND.AbNormal*glbi_testd$REG_FEE

#Sale condition adjland and Registration interaction
glbi_traind$Regscadl= glbi_traind$SALE_COND.AdjLand*glbi_traind$REG_FEE
glbi_testd$Regscadl= glbi_testd$SALE_COND.AdjLand*glbi_testd$REG_FEE

#Sale condition family and Registration interaction
glbi_traind$Regscfam= glbi_traind$SALE_COND.Family*glbi_traind$REG_FEE
glbi_testd$Regscfam= glbi_testd$SALE_COND.Family*glbi_testd$REG_FEE

#Park facil no and Registration interaction
glbi_traind$Regpfno= glbi_traind$PARK_FACIL.No*glbi_traind$REG_FEE
glbi_testd$Regpfno= glbi_testd$PARK_FACIL.No*glbi_testd$REG_FEE

#Park facil yes and Registration interaction
glbi_traind$Regpfyes= glbi_traind$PARK_FACIL.Yes*glbi_traind$REG_FEE
glbi_testd$Regpfyes= glbi_testd$PARK_FACIL.Yes*glbi_testd$REG_FEE

#Date_build and Registration interaction
glbi_traind$Regdbld= glbi_traind$DATE_BUILD*glbi_traind$REG_FEE
glbi_testd$Regdbld= glbi_testd$DATE_BUILD*glbi_testd$REG_FEE

#Buildtype commercial and Registration interaction
glbi_traind$Regbtc= glbi_traind$BUILDTYPE.Commercial*glbi_traind$REG_FEE
glbi_testd$Regbtc= glbi_testd$BUILDTYPE.Commercial*glbi_testd$REG_FEE

#Buildtype house and Registration interaction
glbi_traind$Regbtho= glbi_traind$BUILDTYPE.House*glbi_traind$REG_FEE
glbi_testd$Regbtho= glbi_testd$BUILDTYPE.House*glbi_testd$REG_FEE

#Buildtype others and Registration interaction
glbi_traind$Regbtoth= glbi_traind$BUILDTYPE.Others*glbi_traind$REG_FEE
glbi_testd$Regbtoth= glbi_testd$BUILDTYPE.Others*glbi_testd$REG_FEE

#QS bedroom and Registration interaction
glbi_traind$Regqsbdrm= glbi_traind$QS_BEDROOM*glbi_traind$REG_FEE
glbi_testd$Regqsbdrm= glbi_testd$QS_BEDROOM*glbi_testd$REG_FEE

#QS overall and Registration interaction
glbi_traind$Regqsoa= glbi_traind$QS_OVERALL*glbi_traind$REG_FEE
glbi_testd$Regqsoa= glbi_testd$QS_OVERALL*glbi_testd$REG_FEE
#-------------------------------------------------------------------#

#Area.anna nagar and Commission interaction
glbi_traind$ComAr.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$COMMIS
glbi_testd$ComAr.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$COMMIS

#Area.chrompet and Commission interaction
glbi_traind$ComAr.ch= glbi_traind$AREA.Chrompet*glbi_traind$COMMIS
glbi_testd$ComAr.ch= glbi_testd$AREA.Chrompet*glbi_testd$COMMIS

#Area.karapakkam and Commission interaction
glbi_traind$ComAr.kp= glbi_traind$AREA.Karapakkam*glbi_traind$COMMIS
glbi_testd$ComAr.kp= glbi_testd$AREA.Karapakkam*glbi_testd$COMMIS

#Area.kk nagar and Commission interaction
glbi_traind$ComAr.kkn= glbi_traind$AREA.KK.Nagar*glbi_traind$COMMIS
glbi_testd$ComAr.kkn= glbi_testd$AREA.KK.Nagar*glbi_testd$COMMIS

#Area.T nagar and Commission interaction
glbi_traind$ComAr.tn= glbi_traind$AREA.T.Nagar*glbi_traind$COMMIS
glbi_testd$ComAr.tn= glbi_testd$AREA.T.Nagar*glbi_testd$COMMIS

#Area.Velachery and Commission interaction
glbi_traind$ComAr.vc= glbi_traind$AREA.Velachery*glbi_traind$COMMIS
glbi_testd$ComAr.vc= glbi_testd$AREA.Velachery*glbi_testd$COMMIS

#Sale condition abnormal and Commission interaction
glbi_traind$ComSCabn= glbi_traind$SALE_COND.AbNormal*glbi_traind$COMMIS
glbi_testd$ComSCabn= glbi_testd$SALE_COND.AbNormal*glbi_testd$COMMIS

#Sale condition adjland and Commission interaction
glbi_traind$ComSCadjl= glbi_traind$SALE_COND.AdjLand*glbi_traind$COMMIS
glbi_testd$ComSCadjl= glbi_testd$SALE_COND.AdjLand*glbi_testd$COMMIS

#Sale condition normal sale and Commission interaction
glbi_traind$ComSCns= glbi_traind$SALE_COND.Normal.Sale*glbi_traind$COMMIS
glbi_testd$ComSCns= glbi_testd$SALE_COND.Normal.Sale*glbi_testd$COMMIS

#Park facil no and Commission interaction
glbi_traind$Compfno= glbi_traind$PARK_FACIL.No*glbi_traind$COMMIS
glbi_testd$Compfno= glbi_testd$PARK_FACIL.No*glbi_testd$COMMIS

#Date_build and Commission interaction
glbi_traind$Comdbld= glbi_traind$DATE_BUILD*glbi_traind$COMMIS
glbi_testd$Comdbld= glbi_testd$DATE_BUILD*glbi_testd$COMMIS
#--------------------------------------------------------------#

#Area.Adyar and Registration polynomial interaction
glbi_traind$RegPolAr.ad= glbi_traind$AREA.Adyar*glbi_traind$RegPol
glbi_testd$RegPolAr.ad= glbi_testd$AREA.Adyar*glbi_testd$RegPol

#Area.Anna nagar and Registration polynomial interaction
glbi_traind$RegPolAr.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$RegPol
glbi_testd$RegPolAr.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$RegPol

#Area.KK nagar and Registration polynomial interaction
glbi_traind$RegPolAr.kkn= glbi_traind$AREA.KK.Nagar*glbi_traind$RegPol
glbi_testd$RegPolAr.kkn= glbi_testd$AREA.KK.Nagar*glbi_testd$RegPol

#Area.T nagar and Registration polynomial interaction
glbi_traind$RegPolAr.tn= glbi_traind$AREA.T.Nagar*glbi_traind$RegPol
glbi_testd$RegPolAr.tn= glbi_testd$AREA.T.Nagar*glbi_testd$RegPol

#Sale condition adjland and Registration polynomial interaction
glbi_traind$RegPolscadl= glbi_traind$SALE_COND.AdjLand*glbi_traind$RegPol
glbi_testd$RegPolscadl= glbi_testd$SALE_COND.AdjLand*glbi_testd$RegPol

#Park facil no and Registration polynomial interaction
glbi_traind$RegPolpfno= glbi_traind$PARK_FACIL.No*glbi_traind$RegPol
glbi_testd$RegPolpfno= glbi_testd$PARK_FACIL.No*glbi_testd$RegPol

#Buildtype commercial and Registration polynomial interaction 
glbi_traind$RegPolbtc= glbi_traind$BUILDTYPE.Commercial*glbi_traind$RegPol
glbi_testd$RegPolbtc= glbi_testd$BUILDTYPE.Commercial*glbi_testd$RegPol
#--------------------------------------------------------------------------#
#Area.karapakkam and RegComm interaction
glbi_traind$RegComAr.kp= glbi_traind$AREA.Karapakkam*glbi_traind$RegComm
glbi_testd$RegComAr.kp= glbi_testd$AREA.Karapakkam*glbi_testd$RegComm

#Sale condition adjland and RegComm interaction
glbi_traind$RegComscadl= glbi_traind$SALE_COND.AdjLand*glbi_traind$RegComm
glbi_testd$RegComscadl= glbi_testd$SALE_COND.AdjLand*glbi_testd$RegComm
#--------------------------------------------------------------------------#
#Area.Adyar and Sqft interaction
glbi_traind$AreaSqftad= glbi_traind$AREA.Adyar*glbi_traind$INT_SQFT
glbi_testd$AreaSqftad= glbi_testd$AREA.Adyar*glbi_testd$INT_SQFT

#Area.Anna nagar and Sqft interaction
glbi_traind$AreaSqftan= glbi_traind$AREA.Anna.Nagar*glbi_traind$INT_SQFT
glbi_testd$AreaSqftan= glbi_testd$AREA.Anna.Nagar*glbi_testd$INT_SQFT

#Area.Chrompet and Sqft interaction
glbi_traind$AreaSqftch= glbi_traind$AREA.Chrompet*glbi_traind$INT_SQFT
glbi_testd$AreaSqftch= glbi_testd$AREA.Chrompet*glbi_testd$INT_SQFT

#Area.karapakkam and Sqft interaction
glbi_traind$AreaSqftkp= glbi_traind$AREA.Karapakkam*glbi_traind$INT_SQFT
glbi_testd$AreaSqftkp= glbi_testd$AREA.Karapakkam*glbi_testd$INT_SQFT

#Area.kk nagar and Sqft interaction
glbi_traind$AreaSqftkkn= glbi_traind$AREA.KK.Nagar*glbi_traind$INT_SQFT
glbi_testd$AreaSqftkkn= glbi_testd$AREA.KK.Nagar*glbi_testd$INT_SQFT

#Area.T nagar and Sqft interaction
glbi_traind$AreaSqfttn= glbi_traind$AREA.T.Nagar*glbi_traind$INT_SQFT
glbi_testd$AreaSqfttn= glbi_testd$AREA.T.Nagar*glbi_testd$INT_SQFT

#Bathroom.1 and Sqft interaction
glbi_traind$Sqftbth1= glbi_traind$N_BATHROOM.1*glbi_traind$INT_SQFT
glbi_testd$Sqftbth1= glbi_testd$N_BATHROOM.1*glbi_testd$INT_SQFT

#Buildtype commercial and Sqft interaction
glbi_traind$Sqftbtc= glbi_traind$BUILDTYPE.Commercial*glbi_traind$INT_SQFT
glbi_testd$Sqftbtc= glbi_testd$BUILDTYPE.Commercial*glbi_testd$INT_SQFT

#Buildtype house and Sqft interaction
glbi_traind$Sqftbtho= glbi_traind$BUILDTYPE.House*glbi_traind$INT_SQFT
glbi_testd$Sqftbtho= glbi_testd$BUILDTYPE.House*glbi_testd$INT_SQFT

#QS overall and Sqft interaction
glbi_traind$Sqftqsoa= glbi_traind$QS_OVERALL*glbi_traind$INT_SQFT
glbi_testd$Sqftqsoa= glbi_testd$QS_OVERALL*glbi_testd$INT_SQFT

#Mzzone.a and Sqft interaction
glbi_traind$Sqftmzza= glbi_traind$MZZONE.A*glbi_traind$INT_SQFT
glbi_testd$Sqftmzza= glbi_testd$MZZONE.A*glbi_testd$INT_SQFT

#Mzzone.c and Sqft interaction
glbi_traind$Sqftmzzc= glbi_traind$MZZONE.C*glbi_traind$INT_SQFT
glbi_testd$Sqftmzzc= glbi_testd$MZZONE.C*glbi_testd$INT_SQFT

#Mzzone.RH and Sqft interaction
glbi_traind$Sqftmzzrh= glbi_traind$MZZONE.RH*glbi_traind$INT_SQFT
glbi_testd$Sqftmzzrh= glbi_testd$MZZONE.RH*glbi_testd$INT_SQFT

#Mzzone.RL and Sqft interaction
glbi_traind$Sqftmzzrl= glbi_traind$MZZONE.RL*glbi_traind$INT_SQFT
glbi_testd$Sqftmzzrl= glbi_testd$MZZONE.RL*glbi_testd$INT_SQFT
#-------------------------------------------------------------------#
#Sale condition abnormal and Dist main road interaction
glbi_traind$DMRSCabn= glbi_traind$SALE_COND.AbNormal*glbi_traind$DIST_MAINROAD
glbi_testd$DMRSCabn= glbi_testd$SALE_COND.AbNormal*glbi_testd$DIST_MAINROAD

#Sale condition family and Dist main road interaction
glbi_traind$DMRSCfam= glbi_traind$SALE_COND.Family*glbi_traind$DIST_MAINROAD
glbi_testd$DMRSCfam= glbi_testd$SALE_COND.Family*glbi_testd$DIST_MAINROAD

#Distance main road polynomial term 3 interaction
glbi_traind$DMRPol3= glbi_traind$DIST_MAINROAD^4
glbi_testd$DMRPol3= glbi_testd$DIST_MAINROAD^4
#----------------------------------------------------------------------------#
#Registration polynomial term 2 and Area.Anna nagar interaction
glbi_traind$RegPol2Ar.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$RegPol2
glbi_testd$RegPol2Ar.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$RegPol2

#Registration polynomial term 2 and Area.chrompet interaction
glbi_traind$RegPol2Ar.ch= glbi_traind$AREA.Chrompet*glbi_traind$RegPol2
glbi_testd$RegPol2Ar.ch= glbi_testd$AREA.Chrompet*glbi_testd$RegPol2

#Registration polynomial term 2 and Area.kk nagar interaction
glbi_traind$RegPol2Ar.kkn= glbi_traind$AREA.KK.Nagar*glbi_traind$RegPol2
glbi_testd$RegPol2Ar.kkn= glbi_testd$AREA.KK.Nagar*glbi_testd$RegPol2

#Registration polynomial term 2 and Area.t nagar interaction
glbi_traind$RegPol2Ar.tn= glbi_traind$AREA.T.Nagar*glbi_traind$RegPol2
glbi_testd$RegPol2Ar.tn= glbi_testd$AREA.T.Nagar*glbi_testd$RegPol2

#Registration polynomial term 2 and dist main road interaction 
glbi_traind$RegPol2dmr= glbi_traind$DIST_MAINROAD*glbi_traind$RegPol2
glbi_testd$RegPol2dmr= glbi_testd$DIST_MAINROAD*glbi_testd$RegPol2

#Registration polynomial term 2 and sale condition partial interaction 
glbi_traind$RegPol2SCpar= glbi_traind$SALE_COND.Partial*glbi_traind$RegPol2
glbi_testd$RegPol2SCpar= glbi_testd$SALE_COND.Partial*glbi_testd$RegPol2

#Registration polynomial term 2 and buildtype commercial interaction 
glbi_traind$RegPol2btc= glbi_traind$BUILDTYPE.Commercial*glbi_traind$RegPol2
glbi_testd$RegPol2btc= glbi_testd$BUILDTYPE.Commercial*glbi_testd$RegPol2

#Registration polynomial term 2 and Mzzone.A interaction 
glbi_traind$RegPol2mzza= glbi_traind$MZZONE.A*glbi_traind$RegPol2
glbi_testd$RegPol2mzza= glbi_testd$MZZONE.A*glbi_testd$RegPol2

#Registration polynomial term 2 and Mzzone.C interaction 
glbi_traind$RegPol2mzzc= glbi_traind$MZZONE.C*glbi_traind$RegPol2
glbi_testd$RegPol2mzzc= glbi_testd$MZZONE.C*glbi_testd$RegPol2

#Registration polynomial term 2 and Mzzone.C interaction 
glbi_traind$RegPol2mzzi= glbi_traind$MZZONE.I*glbi_traind$RegPol2
glbi_testd$RegPol2mzzi= glbi_testd$MZZONE.I*glbi_testd$RegPol2

#Registration polynomial term 2 and Mzzone.RH interaction 
glbi_traind$RegPol2mzzrh= glbi_traind$MZZONE.RH*glbi_traind$RegPol2
glbi_testd$RegPol2mzzrh= glbi_testd$MZZONE.RH*glbi_testd$RegPol2

#Registration polynomial term 2 and Mzzone.RM interaction 
glbi_traind$RegPol2mzzrm= glbi_traind$MZZONE.RM*glbi_traind$RegPol2
glbi_testd$RegPol2mzzrm= glbi_testd$MZZONE.RM*glbi_testd$RegPol2

#Registration polynomial term 2 and Commission interaction 
glbi_traind$RegPol2com= glbi_traind$COMMIS*glbi_traind$RegPol2
glbi_testd$RegPol2com= glbi_testd$COMMIS*glbi_testd$RegPol2
#-------------------------------------------------------------------------#
#QS bathroom polynomial term
glbi_traind$QSbthPol= glbi_traind$QS_BATHROOM^2
glbi_testd$QSbthPol= glbi_testd$QS_BATHROOM^2

#QS bathroom polynomial term 8
glbi_traind$QSbthPol8= glbi_traind$QS_BATHROOM^8
glbi_testd$QSbthPol8= glbi_testd$QS_BATHROOM^8

#QS bathroom polynomial term and Area.Adyar interaction
glbi_traind$QSbthPolAr.ad= glbi_traind$AREA.Adyar*glbi_traind$QSbthPol
glbi_testd$QSbthPolAr.ad= glbi_testd$AREA.Adyar*glbi_testd$QSbthPol

#QS bathroom polynomial term and Area.Anna nagar interaction (minimal improvement)
glbi_traind$QSbthPolAr.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$QSbthPol
glbi_testd$QSbthPolAr.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$QSbthPol

#QS bathroom polynomial term and Area.chrompet interaction (minimal improvement)
glbi_traind$QSbthPolAr.ch= glbi_traind$AREA.Chrompet*glbi_traind$QSbthPol
glbi_testd$QSbthPolAr.ch= glbi_testd$AREA.Chrompet*glbi_testd$QSbthPol
#----------------------------------------------------------------------------#
#Registration polynomial term 3 and Area.Adyar interaction
glbi_traind$RegPol3Ar.ad= glbi_traind$AREA.Adyar*glbi_traind$RegPol3
glbi_testd$RegPol3Ar.ad= glbi_testd$AREA.Adyar*glbi_testd$RegPol3

#Registration polynomial term 3 and Area.Anna nagar interaction
glbi_traind$RegPol3Ar.an= glbi_traind$AREA.Anna.Nagar*glbi_traind$RegPol3
glbi_testd$RegPol3Ar.an= glbi_testd$AREA.Anna.Nagar*glbi_testd$RegPol3

#Registration polynomial term 3 and Area.chrompet interaction
glbi_traind$RegPol3Ar.ch= glbi_traind$AREA.Chrompet*glbi_traind$RegPol3
glbi_testd$RegPol3Ar.ch= glbi_testd$AREA.Chrompet*glbi_testd$RegPol3

#Registration polynomial term 3 and Area.kk nagar interaction
glbi_traind$RegPol3Ar.kkn= glbi_traind$AREA.KK.Nagar*glbi_traind$RegPol3
glbi_testd$RegPol3Ar.kkn= glbi_testd$AREA.KK.Nagar*glbi_testd$RegPol3

#Registration polynomial term 3 and Area.t nagar interaction
glbi_traind$RegPol3Ar.tn= glbi_traind$AREA.T.Nagar*glbi_traind$RegPol3
glbi_testd$RegPol3Ar.tn= glbi_testd$AREA.T.Nagar*glbi_testd$RegPol3

#Registration polynomial term 3 and dist main road interaction
glbi_traind$RegPol3dmr= glbi_traind$DIST_MAINROAD*glbi_traind$RegPol3
glbi_testd$RegPol3dmr= glbi_testd$DIST_MAINROAD*glbi_testd$RegPol3

#Registration polynomial term 3 and sale condition partial interaction
glbi_traind$RegPol3SCpar= glbi_traind$SALE_COND.Partial*glbi_traind$RegPol3
glbi_testd$RegPol3SCpar= glbi_testd$SALE_COND.Partial*glbi_testd$RegPol3

#Registration polynomial term 3 and buildtype commercial interaction
glbi_traind$RegPol3SCbtc= glbi_traind$BUILDTYPE.Commercial*glbi_traind$RegPol3
glbi_testd$RegPol3SCbtc= glbi_testd$BUILDTYPE.Commercial*glbi_testd$RegPol3

#Registration polynomial term 3 and Mzzone.A interaction
glbi_traind$RegPol3SCmzza= glbi_traind$MZZONE.A*glbi_traind$RegPol3
glbi_testd$RegPol3SCmzza= glbi_testd$MZZONE.A*glbi_testd$RegPol3

#Registration polynomial term 3 and Mzzone.C interaction
glbi_traind$RegPol3SCmzzc= glbi_traind$MZZONE.C*glbi_traind$RegPol3
glbi_testd$RegPol3SCmzzc= glbi_testd$MZZONE.C*glbi_testd$RegPol3

#Registration polynomial term 3 and Mzzone.I interaction
glbi_traind$RegPol3SCmzzi= glbi_traind$MZZONE.I*glbi_traind$RegPol3
glbi_testd$RegPol3SCmzzi= glbi_testd$MZZONE.I*glbi_testd$RegPol3

#Registration polynomial term 3 and Mzzone.RH interaction
glbi_traind$RegPol3SCmzzrh= glbi_traind$MZZONE.RH*glbi_traind$RegPol3
glbi_testd$RegPol3SCmzzrh= glbi_testd$MZZONE.RH*glbi_testd$RegPol3

#Registration polynomial term 3 and qs room interaction
glbi_traind$RegPol3SCqsrm= glbi_traind$QS_ROOMS*glbi_traind$RegPol3
glbi_testd$RegPol3SCqsrm= glbi_testd$QS_ROOMS*glbi_testd$RegPol3

#Registration polynomial term 3 and Commission interaction
glbi_traind$RegPol3com= glbi_traind$COMMIS*glbi_traind$RegPol3
glbi_testd$RegPol3com= glbi_testd$COMMIS*glbi_testd$RegPol3

glbi_traind= glbi_traind[-160]
glbi_testd= glbi_testd[-159]

#tail(colnames(glbi_traind))
#tail(colnames(glbi_testd))
##Build lm model on Train data
#library(car)
set.seed(123)
index1= sample(7109,5331)
train= glbi_train[index1,]
validation= glbi_train[-index1,]
glbi_lm= lm(SALES_PRICE~., data = train)
summary(glbi_lm)
qqplot(glbi_lm$residuals,glbi_lm$fitted.values)
plot(glbi_lm)

glbi_lm_val_pred= predict(glbi_lm, newdata = validation)
Smy_RMSE(glbi_lm_val_pred)

##Predict using the lm model on test data
glbi_lm_pred= predict(glbi_lm, newdata = glbi_testd)
sd(glbi_lm_pred)
round(sum(abs(glbi_lm_pred- mean(glbi_lm_pred)))/NROW(glbi_lm_pred),2)
output_xls(glbi_lm_pred)

#Use StepAIC to do variable selection
library(MASS)
glbi_lm_step=stepAIC(glbi_lm,trace = F,direction = 'both')
glbi_lm_step$anova

#Use StepAIC model
glbi_lm_aic= lm(SALES_PRICE ~ AREA.Adyar + AREA.Anna.Nagar + AREA.Chrompet + 
AREA.Karapakkam + AREA.KK.Nagar + AREA.T.Nagar + INT_SQFT + 
DATE_SALE + DIST_MAINROAD + N_BEDROOM.2 + N_BEDROOM.3 + N_BEDROOM.4 + 
N_BATHROOM.1 + N_ROOM.3 + SALE_COND.AbNormal + SALE_COND.AdjLand + 
SALE_COND.Family + SALE_COND.Normal.Sale + PARK_FACIL.No + 
DATE_BUILD + BUILDTYPE.Commercial + BUILDTYPE.House + UTILITY_AVAIL.AllPub + 
UTILITY_AVAIL.ELO + UTILITY_AVAIL.NoSeWa + STREET.Gravel + 
STREET.No.Access + MZZONE.A + MZZONE.C + MZZONE.I + MZZONE.RH + 
MZZONE.RL + QS_OVERALL + REG_FEE + COMMIS, data = train)
summary(glbi_lm_aic)

glbi_lm_aic_val_pred= predict(glbi_lm_aic, newdata = validation)
Smy_RMSE(glbi_lm_aic_val_pred)

#Save the preprocessed train and test with all interaction variables in xls
write.csv(glbi_testd,'C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/Greak Lakes Hackathon - Beyond Infinity/glbi_testd3.csv',row.names = F)

#-------------------------------------------------------------------------------#
##The Linear regression model hits an rmse plateau of 225,000 so we went ahead
##and tried to tune Deep Learning model for further improvement in RMSE
library(h2o)
## Create an H2O cloud 
h2o.init(nthreads=-1,            ## -1: use all available threads
         max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll()

#Load a file from disk
glbi_train_h2o= h2o.importFile(path = normalizePath("C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/Greak Lakes Hackathon - Beyond Infinity/glbi_traind.csv"))
glbi_test_h2o= h2o.importFile(path = normalizePath("C:/Users/SUBHRANIL ROY/Desktop/Machine Learning books/ML Data Sets/AV Practice datasets/AV Hackathon Practice/Greak Lakes Hackathon - Beyond Infinity/glbi_testd.csv"))
dim(glbi_train_h2o)
dim(glbi_test_h2o)

#Assignment within H2o
View(glbi_train_h2o)
head(glbi_train_h2o)
str(glbi_train_h2o)
str(glbi_test_h2o)

#Split the data in H2o
splits= h2o.splitFrame(glbi_train_h2o, ratios = c(0.75), seed = 1234)
train1= h2o.assign(splits[[1]], "train1.hex")
validation= h2o.assign(splits[[2]],"validation.hex")
test1= h2o.assign(glbi_test_h2o, "test1.hex")

nrow.H2OFrame(train1)
nrow.H2OFrame(validation)
nrow.H2OFrame(test1)

#Create Predictor and Response variables
response= 'SALES_PRICE'
predictors= setdiff(names(glbi_train_h2o), response)

#Try the first model with 1 epoch
m1= h2o.deeplearning(model_id = 'dl_model_first', training_frame = train1,
                     validation_frame = validation, x= predictors, y= response, activation = 'Rectifier',
                     hidden = c(20,20), epochs = 1, variable_importances = T)
summary(m1)
plot(m1)
head(as.data.frame(h2o.varimp(m1)))

#Try next model with better parameters
m2= h2o.deeplearning(model_id = 'dl_model_second', training_frame = train1,
                     validation_frame = validation, x= predictors, y= response,activation = 'Rectifier',
                     hidden = c(25,25), epochs = 1000000, stopping_rounds = 5, 
                     stopping_metric = 'RMSE', stopping_tolerance = 0.01)
summary(m2)
plot(m2)

head(as.data.frame(h2o.varimp(m2)))

##Random Search for hyper parameter tuning
hyper_params = list(activation=c('Maxout'),
                    hidden=list(c(31,36)),
                    input_dropout_ratio=seq(0,1,0.01),l1=seq(0,1e-4,1e-6),l2=seq(0,1e-4,1e-6))
hyper_params

#Describe the search criteria
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, 
                       max_models = 150, seed=1234567, stopping_rounds=7, stopping_tolerance=1e-2)

#Define the grid
dl_random_grid= h2o.grid(grid_id = 'dl_grid_random30', algorithm = 'deeplearning',
                         training_frame= train1, validation_frame= validation, x=predictors, y=response,
                         epochs=200, stopping_metric='RMSE', stopping_tolerance=1e-2, stopping_rounds=7,
                         max_w2=10, hyper_params = hyper_params,search_criteria = search_criteria)

#Display the grid output
grid = h2o.getGrid("dl_grid_random30",sort_by="RMSE",decreasing=FALSE)
grid@summary_table[1,]

#Build the model with the best parameters
#This model often requires multiple iterations of execution to reach the optimal 
#accuracy level due to the fact that gradient descent can converge to any local 
#minima at each iteration and from the plot we can see there are multiple possible 
#local minima in the error function distribution
#After tuning through Random Search for sufficiently large no of times we got the 
#below architecture which gave optimal accuracy
#This model too did not give optimal accuracy in 1 single iteration
#rather we had to execute 17-18 iterations of running the below model to obtain
#the lowest RMSE value

set.seed(1234)
m3= h2o.deeplearning(model_id = 'dl_model_third', training_frame = train1,
                     validation_frame = validation, x= predictors, y= response,activation = 'Rectifier',
                     hidden = c(32,39), epochs = 100000, input_dropout_ratio = 0.0, 
                     stopping_rounds = 7, stopping_metric = 'RMSE', stopping_tolerance = 0.01)
summary(m3)
plot(m3)

head(as.data.frame(h2o.varimp(m3)))

#Check performance on random 30% & 70% of the data of validation set
for(i in 1:7){
  index= sort(sample(1770,600), decreasing = F)
  validation_30= validation[index,]
  validation_70= validation[-index,]
  y1=h2o.performance(m3, newdata = validation_30)
  y2=h2o.performance(m3, newdata = validation_70)
  print(paste(round(y1@metrics$RMSE,2),round(y2@metrics$RMSE,2)))
}

#Compare performance on train & validation set
h2o.performance(m3, newdata = validation)
h2o.performance(m3, newdata = glbi_train_h2o)

#Predict using the model
glbi_dlm3_pred=h2o.predict(m3, newdata = test1)
sd(glbi_dlm3_pred)
sum(abs(glbi_dlm3_pred- mean(glbi_dlm3_pred)))/nrow.H2OFrame(glbi_dlm3_pred)

x=as.data.frame(glbi_dlm3_pred)
output_xls(x$predict)

#h2o.shutdown()

##----------------------------------VISUALISATION--------------------------##
#glbi_train=na.omit(glbi_train)
#write.csv(glbi_train,'glbi_train.csv')
str(glbi_train)
glbi_train=read.csv('glbi_train.csv')
#Plot Overall Sales Price wrt Area
library(sqldf)
y=sqldf('select Area,avg(Sales_Price) as Sales_Price from glbi_train group by Area')
library(lattice)
barchart(Sales_Price~AREA, data=y)

#Plot Per Sqr ft Price wrt Area
y1=sqldf('select Area,avg(Sales_Price)/avg(INT_SQFT) as Price_per_Sqft,
avg(COMMIS)/avg(INT_SQFT) as Commis_per_Sqft,SALE_COND as Condition, QS_OVERALL
from glbi_train group by Area')
barchart(Price_per_Sqft~AREA, data=y1)
barchart(Commis_per_Sqft~AREA, data = y1)
barchart(Condition~AREA, data=y1)
scatter.smooth(subset(glbi_train,glbi_train$AREA=='Adyar',c(QS_OVERALL)))

barplot(table(sqldf('select MZZONE from glbi_train where Area="Adyar" group by Area')))

library(ggmap)
qmap(location = '', zoom=12)
mydata=y$AREA
for (i in 1:NROW(mydata)){
  latlon=geocode(mydata[i])
  mydata$lon[i]= as.numeric(latlon[1])
  mydata$lat[i]= as.numeric(latlon[2])
}

india_center=as.numeric(geocode('India'))
INDIAMap = ggmap(get_googlemap(center=india_center, scale=2, zoom=4), extent="normal")

location_lat_long= read.csv('location lat long.csv')
draw_geo_map=data.frame(y$AREA, y$Sales_Price,location_lat_long$Long, location_lat_long$Lat)
colnames(draw_geo_map)=c('Area','Sales_Price','Long','Lat')

circle_scale_amt=1
#library(ggplot2)
INDIAMap +
geom_point(aes(x=Long, y=Lat), data=draw_geo_map, alpha=0.4, size=draw_geo_map$Sales_Price*circle_scale_amt) + 
scale_size_continuous(range=range(draw_geo_map$Sales_Price))

ChennaiMap= get_map(location = 'Chennai', zoom = 15)
ggmap(ChennaiMap) + 
geom_point(aes(x = Long, y = Lat), 
data = draw_geo_map,alpha = 0.2, na.rm = T) + 
scale_color_gradient(low = 'beige', high = 'blue')
?geom_point


ggmap(ChennaiMap) + 
geom_density2d(data = draw_geo_map, aes(x = Long, y = Lat), size = 0.3) + 
stat_density2d(data = draw_geo_map, aes(x = Long, y = Lat, fill = ..level.., 
alpha = ..level..), size = 0.01,bins = 16, geom = "polygon") + 
scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)

var(draw_geo_map$Long)
draw_geo_map$Long=c(79,80,81,79.5,80.5,81.5,82)
draw_geo_map$Lat=c(12,12.5,13,13.5,11.5,12,14)

library(plotly)
data("volcano")
p = plot_ly(z = ~volcano, type = "contour")
p
str(volcano)
summary(volcano)
class(volcano)

y3= data.frame(glbi_train$REG_FEE,glbi_train$COMMIS,glbi_train$SALES_PRICE)
p1 = plot_ly(y3, x = ~glbi_train.REG_FEE, y = ~glbi_train.COMMIS, 
z = ~glbi_train.SALES_PRICE, type = "contour", width = 600, height = 500)
p1












































































































































