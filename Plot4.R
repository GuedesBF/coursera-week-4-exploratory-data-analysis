# Plot 4

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

#  Q4
#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

#create index of SCCs that contains "coal" and "combustion" words in the description:

scc<-scc%>%mutate(across(where(is.factor), as.character))
index<-grepl('comb', scc$EI.Sector, ignore.case = TRUE) & grepl('coal', scc$EI.Sector, ignore.case = TRUE)

#Get the SCCs that correspond to the index

index_of_iccs<-scc$SCC[index]

#Subset 'nei' with index_of_iccs

plot4_df<-nei%>%filter(SCC %in% index_of_iccs)%>%group_by(year)%>%summarise(total_emissions=sum(Emissions))

png("Plot4.png")
ggplot(plot4_df, aes(x=year, y=total_emissions))+
        geom_col()+
        ylab('Total emissions')+
        xlab("Year")+
        ggtitle("Total emissions of coal combustion-related sources, USA, 1999-2008")+
        scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))
dev.off()
