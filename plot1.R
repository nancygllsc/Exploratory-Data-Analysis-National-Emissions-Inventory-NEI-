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
plot(DF$YearsData,DF$Means,type = "l",xlab = "Years",ylab = "Emissions Means", main = "Emissions 1999 - 2008")
dev.copy(png, file = "plot1.png")
dev.off()
