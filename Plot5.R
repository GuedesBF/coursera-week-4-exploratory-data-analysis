# Plot 5

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

#  Q5
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

##create index of SCC
index2<-grep('Vehicle', scc$SCC.Level.Two, ignore.case = TRUE)

index_of_iccs_2<-scc$SCC[index2]

#Subset with index_of_iccs_2

plot5_df<-nei%>%filter(SCC %in% index_of_iccs_2 & (fips == "24510"))%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

#Plot

png("Plot5.png")
ggplot(plot5_df, aes(x=year, y=total_emissions))+
        geom_col()+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Motor vehicle emissions, Baltimore, 1999-2008")+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))
dev.off()
