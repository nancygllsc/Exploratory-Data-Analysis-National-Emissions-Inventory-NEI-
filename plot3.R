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
plot3<- ggplot(df)+geom_point(mapping = aes(x =year, y = Emissions)) + facet_grid( ~ type)+labs(title = "Emissions Types from 1999–2008 for Baltimore City")
plot3
dev.copy(png, file = "plot3.png")
dev.off()