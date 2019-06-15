library(xml2)

# Read in xml file  
xml_file <- read_xml("https://data.cdc.gov/api/views/w9j2-ggv5/rows.xml?accessType=DOWNLOAD")

xml_contents(xml_file)
# Check the xml structure size
xml_length(xml_file)
xml_length(xml_children(xml_file))
xml_length(xml_child(xml_children(xml_file), 1))
xml_length(xml_child(xml_child(xml_children(xml_file), 1)))

# Check if each child has same amount of variables
max(xml_length(xml_children(xml_children(xml_file))))
min(xml_length(xml_children(xml_children(xml_file))))

# Set number of columns for final dataframe
ncols <- max(xml_length(xml_children(xml_children(xml_file))))

# Find the index of first element whose length is ncols
ind_name <- which(xml_length(xml_children(xml_children(xml_file)))==5)[1]

# Change it to a large list
xml_list <- as_list(xml_file)

# Check the list size
length(xml_list)
length(xml_list[[1]])
length(xml_list[[1]][[1]])
length(xml_list[[1]][[1]][[2]])

# use For-loop to combine list to a dataframe
deathDF <- data.frame(matrix(ncol = ncols, nrow = 0))
colnames(deathDF) <- names(xml_list[[1]][[1]][[ind_name]])
for (i in 1:length(xml_list[[1]][[1]])){
  tmp <- as.data.frame(xml_list[[1]][[1]][[i]])
  colnames(tmp) <- names(xml_list[[1]][[1]][[i]])
  deathDF <- merge(deathDF, tmp, all = TRUE)
}
View(deathDF)

# create a table
library(tidyverse)
names(deathDF)<- c("Year", "Race", "Sex", "Mortality", "Life Expectancy")
library(DT)
datatable(deathDF, rownames = FALSE)

# create a new varibale
library(tidyverse)
deathDF$`Life Expectancy` <- as.numeric(deathDF$`Life Expectancy`)
deathDF1 <- deathDF %>% group_by(Year) %>% mutate(Average_life_by_year=mean(`Life Expectancy`)) %>% mutate(life_expectancy_diff=`Life Expectancy`-Average_life_by_year)

view(deathDF1)

# when I did the plotting part, I found I need a new categorical variable to make th plot more meaningful
deathDF2<-deathDF1 %>% select(everything()) %>% mutate(Age =ifelse(`Life Expectancy`<= 20,"0-20",
                         ifelse(`Life Expectancy`<=40,"21-40",
                                ifelse(`Life Expectancy`<=60, "41-60",
                                       ifelse(`Life Expectancy`<=80, "61-80", 
                                              ifelse(`Life Expectancy`>80, "older than 80", NA))))))
View(deathDF2)

# plots
library(ggplot2)
library(knitr)

deathDF2$Mortality <- as.numeric(as.character(deathDF2$Mortality))

# create a histogram to see the distribution of life expectancy
g<- ggplot(deathDF, aes(x=`Life Expectancy`))
g+ geom_histogram(color = "grey", fill = "red",size = 1, binwidth = 3)+
  labs(x="Life Expectancy")

# create a side by side bar plot of death rate by age for differetn races 
g <- ggplot(data=deathDF2, aes(x=Race))
g + geom_bar(aes(fill = as.factor(Age)), position = "dodge")+
  labs(x= "Race")+
  scale_fill_discrete(name = "Death rate by age")+
  ggtitle(" Death rate by age for different races")

# create a side by side bar plot of death rate for races and sex
g <- ggplot(data=deathDF2, aes(x=Sex))
g + geom_bar(aes(fill = as.factor(Age)), position = "dodge")+
  labs(x= "Sex")+
  scale_fill_discrete(name = "Death rate by age")+
  facet_grid(~Race)+
  scale_x_discrete(labels = c("Both Sex","F","M"))

# create a side by side boxplot for races and mortality
g <- ggplot(deathDF2, aes(x=Race, y=Mortality))
g+geom_boxplot()+
  ggtitle("Boxplot for Mortality")

# create scatter plots with coloring for mortality and life expectancy
deathDF2$Year<-as.numeric(as.character(deathDF2$Year))
deathDF2$Year
g <- ggplot(deathDF2, aes(x= Year, y=`Life Expectancy`, col=Race))
g+ geom_point(size = 0.5, alpha = 0.6)+
  geom_smooth(aes(group=Race), method = lm, col="black")+
  labs(y="Life Expectancy")
  ggtitle("Mortality vs Life Expectancy")

