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
plot6<-ggplot(MVDF)+geom_point(mapping = aes(x = year, y = Emissions)) + facet_grid( fips~ type)+labs(title = "Baltimore VS Los Angeles Emissions from Motor Vehicle Sources from 1999–2008")
plot6
dev.copy(png, file = "plot6.png")
dev.off()



