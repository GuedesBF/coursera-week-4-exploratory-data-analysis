#My script

library(dplyr)
library(ggplot2)

#Download, unzip and load data:
if (!file.exists("summarySCC_PM25.rds")){
        url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url = url, destfile = 'raw_data.zip')
        unzip('raw_data.zip')
}

nei<-tibble(readRDS("summarySCC_PM25.rds"))
scc<-tibble(readRDS("Source_Classification_Code.rds"))

#  Q1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

plot1_df<-nei%>%filter(Pollutant=='PM25-PRI')%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

with(plot1_df, barplot(total_emissions~year, ylab = 'Total PM2.5 emissions',
                    main = 'Total PM 2.5 emissionsin the United States, 1999 to 2008'))


#  Q2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"|}fips == "24510")
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

plot2_df<-nei%>%filter(Pollutant=='PM25-PRI', fips=="24510")%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

with(plot2_df, barplot(total_emissions~year, type='l', ylab = 'Total PM2.5 emissions',
                    main = 'Total PM2.5 emissionsin the Baltimore City, 1999 to 2008'))

#   Q3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


plot3_df<-nei%>%filter(fips == "24510")%>%
        group_by(year, type)%>%summarise(total_emissions=sum(Emissions))

ggplot(plot3_df, aes(x=year, y=total_emissions, col=type, fill=type))+
        geom_col()+
        ylab('total emissions')+
        facet_grid(.~type)+
        ggtitle('Total emissions of the types "point, nonpoint, onroad, nonroad" in Baltimore, 1999-2008')+
        scale_x_continuous(name="Year", breaks=c(1999, 2002, 2005, 2008))

#  Q4
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

#create index of SCCs that contains "coal" and "combustion" words in the description:

scc<-scc%>%mutate(across(where(is.factor), as.character))
index<-grepl('comb', scc$EI.Sector, ignore.case = TRUE) & grepl('coal', scc$EI.Sector, ignore.case = TRUE)

#Get the SCCs that correspond to the index

index_of_iccs<-scc$SCC[index]

#Subset 'nei' with index_of_iccs

plot4_df<-nei%>%filter(SCC %in% index_of_iccs)%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

ggplot(plot4_df, aes(x=year, y=total_emissions))+
        geom_col()+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Total emissions of coal combustion-related sources, USA, 1999-2008")+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))

#  Q5
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

##create index of SCC
index2<-grep('Vehicle', scc$SCC.Level.Two, ignore.case = TRUE)

index_of_iccs_2<-scc$SCC[index2]

#Subset with index_of_iccs_2

plot5_df<-nei%>%filter(SCC %in% index_of_iccs_2 & (fips == "24510"))%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

ggplot(plot5_df, aes(x=year, y=total_emissions))+
        geom_col()+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Motor vehicle emissions, Baltimore, 1999-2008")+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))

#  Q6
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

#Creating index of iccs of motor vehicles

plot6_df<-plot6_df%>%group_by(city)%>%group_by(city)%>%mutate(change=c(0, ((total_emissions)-lag(total_emissions))[-1]))%>%
        mutate(cummulative_change=cumsum(change), total_relative_change=(cummulative_change/total_emissions[1]))

plot6_df

plot6_df<-nei%>%filter(SCC %in% index_of_iccs_2, fips %in% c("06037","24510"))%>%group_by(year, fips)%>%summarise(total_emissions=sum(Emissions))
plot6_df<-plot6_df%>%mutate(city=ifelse(fips=='06037', 'Los Angeles', "Baltimore"))
plot6.1<-ggplot(plot6_df, aes(x=year, y=total_emissions, col=city, fill=city))+
        geom_col()+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Motor vehicle emissions, LA and Baltimore, 1999-2008")+
        facet_grid(.~city)+
        theme(legend.position = 'none')

#plot6.2<-
plot6.2<-plot6_df%>%group_by(city)%>%ggplot(aes(y=cummulative_change, x=year, fill=ifelse(cummulative_change>0, 'increase', 'decrease')))+
        geom_col()+
        facet_grid(.~city)+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))+
        ylab('cummulative change')+
        labs(fill='direction of change')+
        ggtitle('Cummulative change since 1999')


library(cow_plot)
plot_grid(plot6.1, plot6.2)
cowplot::plot_grid(plot6.1, plot6.2)
