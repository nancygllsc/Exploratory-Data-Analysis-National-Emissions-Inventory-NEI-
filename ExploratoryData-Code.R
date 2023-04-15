#run_analysis.R
# Checking if folder already exists.
install.packages("dplyr")
library(dplyr)
install.packages('datasets')
library(datasets)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#find what pollutants the data contains PM25
unique(NEI$Pollutant)
#find years 
YearsData=as.list(unique(NEI$year))
#names=names(summary(filter(NEI,year==1999)$Emissions))
#USA emissions PM2.5 from 1999 to 2008
Means<-list()
for(x in YearsData){
  print(x)
  df=as.array(c(summary(filter(NEI,year==x)$Emissions)))["Mean"]
  Means<-append(Means,df)
}
DF=data_frame(Means,YearsData)

with(DF,plot(YearsData,Means, type="l"))
plot(DF$YearsData,DF$Means,type = "l",xlab = "Years",ylab = "Emissions Means", main = "USA Emissions 1999 - 2008")
dev.copy(png, file = "plot1.png")
dev.off()



#plot2

install.packages("dplyr")
library(dplyr)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
unique(NEI$Pollutant)
#find years 
YearsData=as.list(unique(NEI$year))
#names=names(summary(filter(NEI,year==1999)$Emissions))
#USA emissions PM2.5 from 1999 to 2008
Means<-list()
for(x in YearsData){
  print(x)
  df=as.array(c(summary(filter(NEI,year==x & fips == "24510")$Emissions)))["Mean"]
  Means<-append(Means,df)
}
DF=data_frame(Means,YearsData)

with(DF,plot(YearsData,Means, type="l"))
plot(DF$YearsData,DF$Means,type = "l",xlab = "Years",ylab = "Emissions Means", main = " ZIP-CODE 24510 Emissions 1999 - 2008")
dev.copy(png, file = "plot2.png")
dev.off()



#plot3
install.packages(c("ggplot2","dplyr"))
library(dplyr)
library(ggplot2)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#find what pollutants the data contains PM25
unique(NEI$Pollutant)
#find years 
years=as.list(unique(NEI$year))
names=names(summary(filter(NEI,year==1999 )$Emissions))
df<- filter(NEI,fips == "24510")
plot3<- ggplot(df)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( ~ type)+labs(title = "Emissions Types from 1999–2008 for Baltimore City")
plot3
dev.copy(png, file = "plot3.png")
dev.off()

#plot4
install.packages(c("ggplot2","dplyr"))
library(dplyr)
library(ggplot2)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#find sectors 
sectors<-list(c(unique(SCC$EI.Sector)))
coal<-list()
for (x in unique(SCC$EI.Sector)){
  for(i in x){
  if(grepl("Coal", i)==TRUE){
    coal<-append(coal,i)
    #print(i)
    
    }
    
  }
  }
coal
#find codes
codesCoal<-list() #should be a list of 99 items
for(e in coal){
  print(e)
  codesCoal<-append(codesCoal,unique(filter(SCC,EI.Sector==e)$SCC))
  
}
length(codesCoal)
#filter data by codes - coal sector 
#create a table from NEI and add rows 
CoalDF<-filter(NEI,SCC=="")
#check for ID matches in the two data sets. 
NEICodes<-unique(c(NEI$SCC))
for(j in codesCoal ){
  for(n in NEICodes ){
  if(grepl(j, n)==TRUE){
    print(j)
    print(n)
  }
}
}
#attach to CoalDF
for(j in codesCoal ){
  print(j)
  dataJoin<-filter(NEI,SCC==j)
  print(dataJoin)
  CoalDF<-rbind(CoalDF,dataJoin)
}

#create graph 
plot4<-ggplot(CoalDF)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( ~ type)+labs(title = "USA Emissions from Coal Combustion-related Sources from 1999–2008")
plot4
dev.copy(png, file = "plot4.png")
dev.off()


#plot 5 motor vehicles pollution - Baltimore 

install.packages(c("ggplot2","dplyr"))
library(dplyr)
library(ggplot2)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#find sectors 
sectors<-list(c(unique(SCC$EI.Sector)))
MotorVehicles<-list()
for (sector in sectors){
  #print(sector)
  for(i in sector){
    
    if(grepl("Mobile", i)==TRUE & i!= "Mobile - Aircraft" & i!= "Mobile - Commercial Marine Vessels" & i!= "Mobile - Locomotives" ){
      
      
        MotorVehicles<-append(MotorVehicles,i)
      #print(i)
      
    }
  }
}

MotorVehicles
#find codes
codesMV<-list() #should be a list of - items
for(sector1 in MotorVehicles){
  print(sector1)
  codesMV<-append(codesMV,unique(filter(SCC,EI.Sector==sector1)$SCC))
  
}
length(codesMV)
#filter data by codes - coal sector 
#create a table from NEI and add rows 
MVDF<-filter(NEI,SCC=="")
#check for ID matches in the two data sets. - 
    #NEICodes<-unique(c(NEI$SCC))
    #for(j in codesMV ){
     # for(n in NEICodes ){
       # if(grepl(j, n)==TRUE){
      #    print(j)
       #   print(n)
      #  }
     # }
    #}
#attach to MVDF
for(j in codesMV ){
  #print(j)
  dataJoin<-filter(NEI,SCC==j & fips == "24510")
  #print(dataJoin)
  MVDF<-rbind(MVDF,dataJoin)
}

#create graph 
plot5<-ggplot(MVDF)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( ~ type)+labs(title = "Emissions from Motor Vehicle Sources from 1999–2008 in Baltimore City")
plot5
dev.copy(png, file = "plot5.png")
dev.off()






#plot6 emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037")



install.packages(c("ggplot2","dplyr"))
library(dplyr)
library(ggplot2)
if(!file.exists("ExploratoryData_NEI.zip")){
  fileURL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileURL,"ExploratoryData_NEI.zip", method = "curl")
}
# Checking if folder exists
if (!file.exists("Source_Classification_Code.rds")) { 
  unzip("ExploratoryData_NEI.zip") 
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#find sectors 
sectors<-list(c(unique(SCC$EI.Sector)))
MotorVehicles<-list()
for (sector in sectors){
  #print(sector)
  for(i in sector){
    
    if(grepl("Mobile", i)==TRUE & i!= "Mobile - Aircraft" & i!= "Mobile - Commercial Marine Vessels" & i!= "Mobile - Locomotives" ){
      
      
      MotorVehicles<-append(MotorVehicles,i)
      #print(i)
      
    }
  }
}
MotorVehicles
#find codes
codesMV<-list() #should be a list of - items
for(sector1 in MotorVehicles){
  print(sector1)
  codesMV<-append(codesMV,unique(filter(SCC,EI.Sector==sector1)$SCC))
  
}
length(codesMV)
#filter data by codes - coal sector 
#create a table from NEI and add rows 
MVDF<-filter(NEI,SCC=="")

#check for ID matches in the two data sets. - 
#NEICodes<-unique(c(NEI$SCC))
#for(j in codesMV ){
# for(n in NEICodes ){
# if(grepl(j, n)==TRUE){
#    print(j)
#   print(n)
#  }
# }
#}
#attach to MVDF
#CA
for(j in codesMV ){
  #print(j)
  dataJoin<-filter(NEI,SCC==j & fips == "06037" )
  #print(dataJoin)
  MVDF<-rbind(MVDF,dataJoin)
}
#MA
for(j in codesMV ){
  #print(j)
  dataJoin<-filter(NEI,SCC==j & fips == "24510" )
  #print(dataJoin)
  MVDF<-rbind(MVDF,dataJoin)
}

#create graph 
plot6<-ggplot(MVDF)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( fips~ type)+labs(title = "Emissions from Motor Vehicle Sources from 1999–2008 in Baltimore City and Los Angeles County California")
plot6
dev.copy(png, file = "plot6.png")
dev.off()







#_________
df<-arrange(SCC,EI.Sector,SCC)

df4<- filter(NEI,==10100101)
plot4<- ggplot()+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( ~ type)
plot4
#
DF<-data.frame(Means=c( Df99,Df02,Df05,Df08) )
DF<-mutate(DF,YearsData=years)

with(DF,plot(YearsData,Means, type="l"))
plot(DF$YearsData,DF$Means,type = "l",xlab = "Years",ylab = "Emissions Zip Code 24510 Means", main = "Emissions Zip Code 24510 During 1999 - 2008")
dev.copy(png, file = "plot2.png")
dev.off()


