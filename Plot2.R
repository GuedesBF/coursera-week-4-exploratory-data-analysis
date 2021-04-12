# Plot 2

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

#  Q2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"|}fips == "24510")
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

plot2_df<-nei%>%filter(Pollutant=='PM25-PRI', fips=="24510")%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

png('Plot2.png')
with(plot2_df, barplot(total_emissions~year, ylab = 'Total PM2.5 emissions',
                       main = 'Total PM2.5 emissionsin the Baltimore City, 1999 to 2008'))
dev.off()
