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
plot4<-ggplot(CoalDF)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( ~ type)+labs(title = "USA Coal Combustion-related Sources Emissions 1999–2008")
plot4
dev.copy(png, file = "plot4.png")
dev.off()
