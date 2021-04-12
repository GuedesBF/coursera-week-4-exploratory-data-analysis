# Plot 6

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

#  Q6
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

#Creating index of iccs of motor vehicles

index2<-grep('Vehicle', scc$SCC.Level.Two, ignore.case = TRUE)
index_of_iccs_2<-scc$SCC[index2]

plot6_df<-nei%>%filter(SCC %in% index_of_iccs_2, fips %in% c("06037","24510"))%>%group_by(year, fips)%>%summarise(total_emissions=sum(Emissions))
plot6_df<-plot6_df%>%mutate(city=ifelse(fips=='06037', 'Los Angeles', "Baltimore"))
plot6_df<-plot6_df%>%group_by(city)%>%group_by(city)%>%mutate(change=c(0, ((total_emissions)-lag(total_emissions))[-1]))%>%
        mutate(cummulative_change=cumsum(change), total_relative_change=(cummulative_change/total_emissions[1]))

#load 'cowplot' package for multi-panel ggplot
library(cowplot)

#create two plots

plot6.1<-ggplot(plot6_df, aes(x=year, y=total_emissions, col=city, fill=city))+
        geom_col()+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Motor vehicle emissions, LA and Baltimore, 1999-2008")+
        facet_grid(.~city)+
        theme(legend.position = 'none')

plot6.2<-plot6_df%>%group_by(city)%>%ggplot(aes(y=cummulative_change, x=year, fill=ifelse(cummulative_change>0, 'increase', 'decrease')))+
        geom_col()+
        facet_grid(.~city)+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))+
        ylab('cummulative change')+
        labs(fill='direction of change')+
        ggtitle('Cummulative change since 1999')

png('Plot6.png')
plot_grid(plot6.1, plot6.2)
dev.off()
