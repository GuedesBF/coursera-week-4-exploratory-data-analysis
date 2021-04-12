# Plot 1

library(dplyr)
library(ggplot2)

#Download, unzip and load data:
if (!file.exists("summarySCC_PM25.rds")){
        url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url = url, destfile = 'raw_data.zip')
        unzip('raw_data.zip')
}

if(!exists('nei')){
        nei<-tibble(readRDS("summarySCC_PM25.rds"))
        scc<-tibble(readRDS("Source_Classification_Code.rds"))
}

#  Q1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

plot1_df<-nei%>%filter(Pollutant=='PM25-PRI')%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

png('Plot1.png')
with(plot1_df, barplot(total_emissions~year, ylab = 'Total PM2.5 emissions',
                       main = 'Total PM 2.5 emissionsin the United States, 1999 to 2008'))
dev.off()
