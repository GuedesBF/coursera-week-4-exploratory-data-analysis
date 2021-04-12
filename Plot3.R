# Plot 3

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

#   Q3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


plot3_df<-nei%>%filter(fips == "24510")%>%
        group_by(year, type)%>%summarise(total_emissions=sum(Emissions))

png("Plot3.png")
ggplot(plot3_df, aes(x=year, y=total_emissions, col=type, fill=type))+
        geom_col()+
        ylab('total emissions')+
        facet_grid(.~type)+
        ggtitle('Total emissions of diferent types in Baltimore, 1999-2008')+
        scale_x_continuous(name="Year", breaks=c(1999, 2002, 2005, 2008))
dev.off()
