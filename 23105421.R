getwd()
setwd("C:/Users/ANUP KAMATH/Desktop")
getwd()
file1<-read.csv(file="C:/Users/ANUP KAMATH/Desktop/children-born-per-woman.csv")
head(file1)
file2<-read.csv(file="C:/Users/ANUP KAMATH/Desktop/world-happiness-report.csv")
head(file2)
file3<-read.csv(file="C:/Users/ANUP KAMATH/Desktop/population_by_country_2020.csv")
head(file3)

#file 2 changes
subset_b_2020 <- file2[file2$year == 2020, ]

# View the subset
head(subset_b_2020)
subset_b_2020 <- subset_b_2020[, c("Country.name", "Healthy.life.expectancy.at.birth", 
                                   "Freedom.to.make.life.choices", "Generosity", 
                                   "Perceptions.of.corruption")]
names(subset_b_2020) <- c("country", "life_expectancy", "freedom","generosity","corruption")

# View the modified subset
head(subset_b_2020)

#file 1 changes
subset_a_2020 <- file1[file1$Year == 2020, ]
head(subset_a_2020)

subset_a_2020 <- subset_a_2020[, c("Entity", "Code", 
                                   "Fertility.rate..Gapminder..v12..2017.")]
names(subset_a_2020) <- c("country", "code", "fertility_rate")
head(subset_a_2020)


#file 3 changes
head(file3)
colnames(file3)
subset_c_2020 <- file3[, c("Country..or.dependency.", "Population..2020.", 
                           "Density..P.Km..")]

names(subset_c_2020)<- c("country", "population", "density_pkm")
head(subset_c_2020)

#joining tables
library(dplyr)



# Convert country column to lowercase in all dataframes
subset_a_2020 <- subset_a_2020 %>% mutate(country = tolower(country))
subset_b_2020 <- subset_b_2020 %>% mutate(country = tolower(country))
subset_c_2020 <- subset_c_2020 %>% mutate(country = tolower(country))


#join data
country_data <- subset_b_2020 %>%
  inner_join(subset_a_2020, by = "country") %>%
  inner_join(subset_c_2020, by = "country")

#show new dataset
head(country_data)

#--------------------#glm model ------------------
#quantitative analysis- glm model
library(dplyr)


# Converting corruption into factor inorder to run glm model
country_data <- country_data %>%
  mutate(corruption_factor = ifelse(corruption > 0.6, 1, 0))

# Convert 'corruption_factor' to a factor variable
country_data$corruption_factor <- factor(country_data$corruption_factor)

head(country_data)

# running glm model
set.seed(100)

# Split data into training and testing sets
sample_indices <- sample(1:nrow(country_data), 70)
test_data <- country_data[sample_indices, ]
train_data <- country_data[-sample_indices, ]

# Fit the logistic regression model
glm_model <- glm(corruption_factor ~ freedom, family = binomial(link = "logit"), data = train_data)

# Summary of the model
summary(glm_model)

# Plot the model
plot(glm_model)

#----------------------------- #gam model------------------------

#gam model
#install.packages("gam")
library(mgcv)

# Fit a GAM model
gam_model <- gam(corruption ~ s(freedom ) + s(generosity), data = country_data)

# Summary of the model
summary(gam_model)

plot(gam_model)


#---------------------#adding latitude and longitude--------------

#instakk required packages
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyverse")

library(readr)
library(dplyr)
library(tidyverse)
library(mgcv)

#if (!require("tm")) install.packages("tm")
#if (!require("wordcloud")) install.packages("wordcloud")
#if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(tm)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tidyr)

if (!require("leaflet")) install.packages("leaflet")
if (!require("countrycode")) install.packages("countrycode")

library(leaflet)
library(countrycode)

#install.packages("plotly")
library(plotly)


head(country_data)
library(maps)
library(ggplot2)

world_map <- map_data("world")

# Convert country names in anuata to lowercase
country_data$country <- tolower(country_data$country)

# Convert region names in world_map (which represents countries here) to lowercase
world_map$region <- tolower(world_map$region)

# unique list of countries with their mean latitude and longitude
country_coords <- world_map %>%
  group_by(region) %>%
  summarize(lat = mean(lat), lon = mean(long), .groups = 'drop')


# Merge the coordinates
country_data <- merge(country_data, country_coords, by.x = "country", by.y = "region", all.x = TRUE)

non_matching <- country_data[is.na(country_data$lat), "country"]
unique(non_matching)
#US and UK latitude and logitude missing

#adding lat & long for US
country_data$lat <- ifelse(country_data$country == "united states", 37.0902, country_data$lat)
country_data$lon <- ifelse(country_data$country == "united states", -95.7129, country_data$lon)

# adding lat & long for Uk
country_data$lat <- ifelse(country_data$country == "united kingdom", 55.3781, country_data$lat)
country_data$lon <- ifelse(country_data$country == "united kingdom", -3.4360, country_data$lon)

tail(country_data)
#---------------------------interactive design---------------------

# https://r-graph-gallery.com/interactive-charts.html
#creating a subset 
selected_countries <- c("Afghanistan", "United States", "China", "Ireland", "India")

# Create a subset where the "Country.name" is in the selected countries list
subset_file2 <- subset(file2, Country.name %in% selected_countries)

# View the subset
print(subset_file2)


#creating interactive graph
library(plotly)
library(dplyr)

# Assuming 'subset_file2' is your dataframe

# Create Plotly plot
fig <- plot_ly(subset_file2, x = ~year, y = ~Healthy.life.expectancy.at.birth, 
               color = ~Country.name, text = ~Country.name) %>%
  add_markers() %>%
  layout(title = "Healthy Life Expectancy at Birth Over Years",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Healthy Life Expectancy at Birth"),
         hovermode = "closest")

# Print the plot
print(fig)
#-----------------------#leaflet analysis---------------------

library(leaflet)

world_map_leaflet <- leaflet(data = country_data) %>%
  addTiles() %>%
  setView(lng = 0, lat = 30, zoom = 2)  


world_map_leaflet <- world_map_leaflet %>%
  addMarkers(
    lng = ~lon,  
    lat = ~lat,  
    popup = ~paste0("<b>Country:</b> ", country, "<br>",
                    "<b>Population:</b> ", population, "<br>",
                    "<b>Corruption:</b> ", corruption)  
  )

# Display the map
world_map_leaflet
#---------------------some analysis--------------------------------------
#Bar chart
library(ggplot2)

# Subset 
top_5_populated <- head(country_data[order(-country_data$population), ], 5)

# Create the bar plot
ggplot(data = top_5_populated, aes(x = country, y = population, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = population), vjust = -0.5, size = 3) +  
  labs(x = "Country", y = "Population", title = "Population of Top 5 Countries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#--------------------------------------------------------------------
#Bar chart 2
ggplot(data = top_5_populated, aes(x = country, y = life_expectancy, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = life_expectancy), vjust = -0.5, size = 3) +  
  labs(x = "Country", y = "Life Expectancy", title = "Life Expectancy of Top 5 populated Countries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#------------------------------------------------------------

# Scatter plot

library(ggplot2)

top_20_populated <- head(country_data[order(-country_data$population), ], 20)  

ggplot(top_20_populated, aes(x = fertility_rate, y = life_expectancy, size = population, color = country)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = country), vjust = -0.5, hjust = 0.5, size = 3) +  # Add country labels
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Fertility Rate vs. Life Expectancy for Selected countries",
       x = "Fertility Rate",
       y = "Life Expectancy") +
  theme_minimal()

#----------------------------------------------------------------
#box plots
library(ggplot2)
library(cowplot)

# Box plot for life expectancy
plot1 <- ggplot(country_data, aes(y = life_expectancy)) +
  geom_boxplot(color = "blue", fill = "lightblue") +
  labs(title = "Box Plot of Life Expectancy")


# Box plot for generosity
plot2 <- ggplot(country_data, aes(y = generosity)) +
  geom_boxplot(color = "red", fill = "pink") +
  labs(title = "Box Plot of Generosity")

# Box plot for corruption
plot3 <- ggplot(country_data, aes(y = corruption)) +
  geom_boxplot(color = "purple", fill = "lavender") +
  labs(title = "Box Plot of Corruption")

# Box plot for fertility rate
plot4 <- ggplot(country_data, aes(y = fertility_rate)) +
  geom_boxplot(color = "orange", fill = "peachpuff") +
  labs(title = "Box Plot of Fertility Rate")

# Arrange plots in one image
plot_grid(plot1, plot2, plot3, plot4, ncol = 2)
#-------------------------------------------------------------
