# R script for Week 5 & 6 exercises.
# Bellevue University
# DSC640-T302
# Garth Scheck
# 2024-09-26

# load libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(treemapify))
suppressPackageStartupMessages(library("ggsci"))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
require(gridExtra)

# read in data
data1 <- read_csv('D:/Statistics/640/Car/kiaHyundaiThefts.csv',show_col_types = FALSE)
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

# plot 1 is a bar chart of KIA thefts by state
plot1 <- ggplot(df, aes(x = state, y = countKiaHyundaiThefts)) +
    geom_bar(stat = "identity", fill = 4) +
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) 

# plot 2 is a bar chart of other vehicle thefts by state
plot2 <- ggplot(df, aes(x = state, y = countOtherThefts)) +
    geom_bar(stat = "identity", fill = 4) +
    coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) 

# format date into Month and Year
df$dt <- paste(df$month, "-", df$year)
df$dt <- as.yearmon(df$dt, format = "%b - %Y")

# get mean of car thefts by date
df_mean <- df %>%
  group_by(dt) %>%
  summarise(mean_value = mean(countOtherThefts))

# plot 3 shows an area plot of the other vehicle thefts over time
plot3 <- ggplot(df_mean, aes(x = dt, y = mean_value)) + 
  		geom_area(fill = 4) +
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) 

# get the mean of KIA thefts by date
kia_mean_df <- df %>%
  group_by(dt) %>%
  summarise(kia_mean = mean(countKiaHyundaiThefts))

# get the mean of other thefts by date
other_mean_df <- df %>%
  group_by(dt) %>%
  summarise(other_mean = mean(countOtherThefts))

new_df <- kia_mean_df
new_df$other_mean <- other_mean_df$other_mean

# plot 4 shows an area plot of KIA and other vehicle thefts over time
plot4 <- ggplot(new_df) + 
  geom_area(aes(x = dt, y = kia_mean),
            fill = 4, alpha = 0.85) +
  geom_area(aes(x = dt, y = other_mean),
            fill = "#003f5c", alpha = 0.35) +
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) 

# read in data
data1 <- read_csv('D:/Statistics/640/Car/KiaHyundaiMilwaukeeData.csv',show_col_types = FALSE)
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

# get sum of thefts
kt = sum(df$countKiaHyundaiThefts)
ot = sum(df$countOtherThefts)

# get percentages
kp = round(kt/(kt+ot), 2)
op = round(ot/(kt+ot), 2)

# put in new dataframe
new_df <- data.frame(value = c(kp, op),
                 group = c("KIA", "Other"))

hsize <- 2.0

new_df <- new_df %>% 
  mutate(x = hsize)

# plot 5 shows a doughnut chart of KIA vs other thefts
plot5 <- ggplot(new_df, aes(x = hsize, y = value , fill = group)) +
  		geom_col() +
  		geom_text(aes(label = value),
             	color = "white",
             	position = position_stack(vjust = 0.5)) +
		labs(x = "", y = "") +
		ggtitle("Car Thefts") +
		theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) +
  		coord_polar(theta = "y") +
		scale_fill_cosmic("signature_substitutions") +
	      xlim(c(0.2, hsize + 0.5))

# read in data
data1 <- read_csv('D:/Statistics/640/Car/kiaHyundaiThefts.csv',show_col_types = FALSE)
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

# pivot data
df <- df %>% pivot_longer(cols=c('countKiaHyundaiThefts','countOtherThefts'), 
				names_to = 'category', 
				values_to = 'thefts')
 
# plot 6 shows a stacked bar chart of KIA vs Other thefts
plot6 <- ggplot(df, aes(fill=category, y=thefts, x=state)) + 
    		geom_bar(position="stack", stat="identity") +
		labs(x = "State", y = "Number Of Thefts") +
		ggtitle("Car Thefts") +
		scale_fill_cosmic("signature_substitutions", labels = c("KIA", "Other")) +
		theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')
	)

# read in data
data1 <- read_csv('D:/Statistics/640/Car/sales/Sales_table.csv',show_col_types = FALSE)
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

# get sums of kia and other car sales
ks = sum(df[which(df$Maker=='KIA'), 4])
os = sum(df[which(df$Maker!='KIA'), 4])

# get percentages
kp = round(ks/(ks+os), 2)
op = round(os/(ks+os), 2)

# store in new dataframe
new_df <- data.frame(value = c(kp, op),
                 group = c("KIA", "Other"))

# plot 7 shows a pie chart of KIA sales vs Other cars
plot7 <- ggplot(new_df, aes(x = "", y = value , fill = group)) +
  		geom_col() +
  		geom_text(aes(label = value),
             	color = "white",
             	position = position_stack(vjust = 0.5)) +
		labs(x = "", y = "") +
		ggtitle("Car Sales") +
		theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) +
  		coord_polar(theta = "y") +
		scale_fill_cosmic("signature_substitutions")

# read in data
data1 <- read_csv('D:/Statistics/640/Car/sales/Sales_table - Copy.csv',show_col_types = FALSE)
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

# get sales of KIA models of more than 10
ks <- subset(df, Maker == "KIA" & twenty_twenty > 10)

# order for pereto
ks$Genmodel <- with(ks, factor(ks$Genmodel, levels=ks[order(-twenty_twenty), ]$Genmodel))

# plot 8 pereto of KIA model sales
plot8 <- ggplot(ks, aes(x = Genmodel, y = twenty_twenty)) +
    geom_bar(stat = "identity", fill = 4) +
    coord_flip() +
	labs(x = "Model", y = "Sales Amount in Dollars") +
	ggtitle("KIA Sales") +
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent')) 




# read in data
data1 <- read_excel('D:/Statistics/640/Car/Motherboard subset.xlsx')
head(data1, 10)

# convert to dataframe
df <- data.frame(data1)

df <- df[complete.cases(df), ]

# plot 9 shows a line plot of Washington KIA thefts
plot9 <- ggplot(df, aes(x)) + 
  geom_line(aes(x = dt, y = Vancouver_Kia_Hyundais, colour = "Vancouver"), size = 1.25) +
  geom_line(aes(x = dt, y = Spokane_Kia_Hyundais, colour = "Spokane"), size = 1.25) +
  scale_colour_manual("", 
                      values = c("Vancouver"="#4682b4", 
					"Spokane"="#98F5FF")) +
  labs(x = "Date", y = "Number of Thefts") +
  ggtitle("Washington KIA Thefts") + 
  theme(plot.title = element_text(hjust = 0.5),
		panel.background = element_rect(fill='transparent'),
         	plot.background = element_rect(fill='transparent', color=NA),
         	panel.grid.major = element_blank(),
         	panel.grid.minor = element_blank(),
         	legend.background = element_rect(fill='transparent'),
         	legend.box.background = element_rect(fill='transparent'))


# place plots in a grid for display, like a dashboard
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=2, nrow=5)

# lastly, read in map data and display on a US map
data2 <- read_csv('D:/Statistics/640/Car/carTheftsMap.csv',show_col_types = FALSE)
head(data2, 10)

df <- data.frame(data2)
mapview(df, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)

