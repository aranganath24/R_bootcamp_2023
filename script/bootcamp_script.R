# Load Libraries
library(tidyverse)
library(wosr)
library(psych)
library(fastDummies)
library(janitor)
library(tidytext)
library(wordcloud2)



# Foundations -------------------------------------------------------------

# R as a calculator

# calculates 2+2
2+2

# calculates 65 to the power of 4
65^4

# Object Assignment: assign value 5 to new object named x
x<-5

# prints value assigned to "x"
x

# assign value 12 to new object named y
y<-12

# prints value assigned to "y"
y

# creates a new object, named "xy_sum" whose value is the sum of "x" and "y"
xy_sum<-x+y

# prints contents of "xy_sum"
xy_sum

# changing/updating value of object: assign value of "8" to object named "x", overwriting previous assignment
x<-8

# prints updated value of "x"
x

# prints value of "xy_sum"; note that the updated value of "x" isn't automatically reflected in "xy_sum"
xy_sum

# assigns sum of "y" and newly updated value of "x" to "xy_sum" object
xy_sum<-x+y

# prints updated value of "xy_sum"
xy_sum

# assigns text string "Boulder, CO" to 
our_location<-"Boulder, CO"

# prints value of "our_location" object
our_location

# case sensitivity
our_Location

# Data Structures

#VECTORS

# makes vector with values 5,7,55,32
c(5, 7, 55, 32)

# assigns vector of arbitrary values to new object named "arbitrary_values"
arbitrary_values<-c(5,7,55.6,32.5)

# prints vector assigned to "arbitrary_values" object
arbitrary_values

# creates a new vector that doubles the values in "arbitrary_values" and assigns it to a new object named
"arbitrary_values_2x"
arbitrary_values_2x<-arbitrary_values*2

# prints contents of "arbitrary_values_2x"
arbitrary_values_2x

# adds "arbitrary_values" vector and "arbitrary_values_2x" vector
arbitrary_values + arbitrary_values_2x

# extracts third element of "arbitrary_values_2x" vector
arbitrary_values[3]

# extracts a new vector comprised of the 2nd, 3rd, and 4th elements of the existing "arbitrary_values" vector
arbitrary_values[2:4]

# creates character vector whose elements are the first four months of the year, and assigns the vector to a new object named "months_four"
months_four<-c("January", "February", "March", "April")

# prints contents of "months_four"
months_four

# extracts second element of "months_four" object (i.e. the "February" string)
months_four[2]

# subsets the second and third elements of "months_four" object (i.e. the "February" and "March" strings, which are extracted as a new character vector)
months_four[2:3]


# Data Frames

# Creates a dummy country-level data frame 
country_df<-data.frame(Country=c("Country A", "Country B", "Country C"),
                       GDP=c(8000, 30000, 23500),
                       Population=c(2000, 5400, 10000),
                       Continent=c("South America", "Europe", "North America"))


# prints "country_df" data frame to console
country_df

# pulls up "country_df" data frame in R Studio data viewer
View(country_df)

# LISTS

# creates list whose elements are the "arbitrary_values" numeric vector, 
# the "months_four" character vector, and the "country_df" data frame,
# and assigns it to a new object named "example_list"
example_list<-list(arbitrary_values, months_four, country_df)

# prints contents of "example_list"
example_list

# extracts country-level data frame from "example_list"; the country-level data frame is 
# the third element in "example_list"
example_list[[3]]

# extracts the first and third elements from "example_list"
example_list[c(1,3)]


# creates a character vector of desired names for list elements, and 
# assigns it to a new object named "name_vector"
name_vector<-c("element1", "element2", "element3")

# assigns names from "name_vector" to list elements in "example_list"
names(example_list)<-name_vector

# prints contents of "example_list"
example_list

# Extracts the data frame from "example_list" by its assigned name
example_list[["element3"]]


# Extracts the "element3" data frame from "example_list" by its index number
example_list[[3]]

# Identifying data structures

# print the data structure of the "example_list" object
class(example_list)

# print the data structure of the "arbitrary_values" object
class(arbitrary_values)



# Functions ---------------------------------------------------------------

# fahrenheit to Celsius formula, where "F" is fahrenheit input
C=(F-32)*(5/9)

# Converts 55 degrees fahrenheit to Celsius
(55-32)*(5/9)

# Generates function that takes fahrenheit value ("fahrenheit_input") and returns a 
# value in Celsius, and assigns the function to an object named "fahrenheit_to_celsius_converter"
fahrenheit_to_celsius_converter<-function(fahrenheit_input){
  celsius_output<-(fahrenheit_input-32)*(5/9)
  return(celsius_output)
}

# Uses "fahrenheit_to_celsius_converter" function to convert 68 degrees Fahrenheit to Celsius
fahrenheit_to_celsius_converter(fahrenheit_input=68)

# Uses "fahrenheit_to_celsius_converter" function to convert 22 degrees Fahrenheit to Celsius
fahrenheit_to_celsius_converter(fahrenheit_input=22)

# Once we have a function written down, it is straightforward to apply that function to multiple 
# inputs in an iterative fashion. For example, let's say you have four different Fahrenheit 
# temperature values that you would like to convert to celsius, using the
# "fahrenheit_to_celsius_converter" we developed above. One option would be to apply the 
# fahrenheit_to_celsius_converter``` function to each of the Fahrenheit temperature inputs
# individually. For example, let's say our Fahrenheit values, which we'd like to convert to Celsius, 
# are the following: 45.6, 95.9, 67.8, 43. We could, of course, run these values through the function 
# individually, as below:

fahrenheit_to_celsius_converter(fahrenheit_input=45.6)

fahrenheit_to_celsius_converter(fahrenheit_input=95.9)

fahrenheit_to_celsius_converter(fahrenheit_input=67.8)

fahrenheit_to_celsius_converter(fahrenheit_input=43.)



# makes a vector out of Fahrenheit values we want to convert, and assigns it to a 
# new object named "fahrenheit_input_vector"
fahrenheit_input_vector<-c(45.6, 95.9, 67.8, 43)

# Iteratively applies the "fahrenheit_to_celsius_converter" to celsius input values in
# "fahrenheit_input_vector" and assigns the resulting vector of converted temperature values to 
# "celsius_ouputs_vector"
celsius_outputs_vector<-map_dbl(fahrenheit_input_vector, fahrenheit_to_celsius_converter)

# prints contents of "celsius_outputs_vector"
celsius_outputs_vector

# iteratively applies the "fahrenheit_to_celsius_converter" function to the input values in 
# "fahrenheit_input_vector", and assigns the list of celsius output values to a new object 
# named "celsius_outputs_list"
celsius_outputs_list<-map(fahrenheit_input_vector, fahrenheit_to_celsius_converter)

# prints contents of "celsius_outputs_list"
celsius_outputs_list

# checks data structure of "celsius_outputs_list"
class(celsius_outputs_list)

# Creates function that takes an input value in degrees Fahrenheit (fahrenheit_input), 
# converts this value to Celsius, and returns a data frame with the input Fahrenheit 
# temperature value as one column, and the corresponding Celsius temperature value as 
# another column; the function is assigned to a new object named "fahrenheit_to_celsius_converter_df" 
fahrenheit_to_celsius_converter_df<-function(fahrenheit_input){
  celsius_output<-(fahrenheit_input-32)*(5/9)
  celsius_output_df<-data.frame(fahrenheit_input, celsius_output)
  return(celsius_output_df)
}


# applies "fahrenheit_to_celsius_converter_df" function to input value of 63 degrees Fahrenheit
fahrenheit_to_celsius_converter_df(fahrenheit_input=63)

# Iteratively applies the "fahrenheit_to_celsius_converter_df" function to input values in 
# "fahrenheit_input_vector" to generate a data frame with column of input Fahrenheit values, 
# and column of corresponding output Celsius values; assigns this data frame to a new object 
# named "celsius_outputs_df"
celsius_outputs_df<-map_dfr(fahrenheit_input_vector, fahrenheit_to_celsius_converter_df)


# prints contents of 
celsius_outputs_df

# pulls up documentation for "map" function
?map


# creates sample country-level Fahrenheit data for Country A
countryA_fahrenheit<-c(55,67,91,23, 77, 98, 27)

# creates sample country-level Fahrenheit data for Country B
countryB_fahrenheit<-c(33,45,11,66, 44)

# creates sample country-level Fahrenheit data for Country C
countryC_fahrenheit<-c(60,55,12,109)

# creates sample country-level Fahrenheit data for Country D
countryD_fahrenheit<-c(76, 24, 77, 78)


# Creates list of input vectors and assigns this list to new object named "input_list"
temperature_input_list<-list(countryA_fahrenheit, countryB_fahrenheit, countryC_fahrenheit, countryD_fahrenheit) 

# Iteratively passes vectors in "temperature_input_list" as arguments to "fahrenheit_to_celsius_converter_df" and deposits the resulting data frames to a list, which is assigned to a new object named "processed_temperature_data_list"
processed_temperature_data_list<-map(temperature_input_list, fahrenheit_to_celsius_converter_df)

# prints contents of "processed_temperature_data_list"
processed_temperature_data_list



# Applied Data Work in R --------------------------------------------------

# Reads in Persson/Tabellini Data from local directory
pt<-read_csv("data/pt/persson_tabellini_workshop.csv")

# Iteratively reads in all individual WOS files from the "data/wos" directory and assigns
# it to an object named "wos_file_list"

# sets working directory to the directory containing WOS files
setwd("data/wos")

# Uses "list.files()" function to create a character vector of file names; assigns
# character vector to new object named "wos_files"
wos_files<-list.files()

# prints contents of "wos_files"
wos_files

# iteratively reads in WOS datasets in working directory to a list that is assigned
# to an object named "wos_file_list"
wos_file_list<-map(wos_files, read_csv)

# prints contents of "wos_file_list"
wos_file_list

# appends data frames in "wos_file_list" into one data frame and assigns it to a new 
# object named "ws_df_appended"
ws_df_appended<-bind_rows(wos_file_list)

# Views "ws_df_appended" in R Studio data viewer
ws_df_appended

# READS IN DATA FROM WOSR PACKAGE

# creates WOS session identifier and assigns to object named "sid"
sid<-auth(username=NULL, password=NULL)

# Creates string for WOS query
wos_query<-'TS = ("climate" & "art") AND PY = (2015-2016)'

# Pulls data from WOS API based on wos_query and assigns to object named "wos_api_climate_art"
wos_api_climate_art<-pull_wos(wos_query, sid=sid)

# checks class of "wos_api_climate_art"
class(wos_api_climate_art)

# prints contents of "wos_api_climate_art"
wos_api_climate_art

# Reads in PT dataset from dropbox and assigns it to a new object named "pt_cloud"
pt_cloud<-read_csv("https://www.dropbox.com/s/iczslf52s8bzku2/persson_tabellini_workshop.csv?dl=1")

# Reads in published dataset from CU Scholar and assigns it to a new object named "green_space_CUScholar"
green_space_CUScholar<-read_csv("https://scholar.colorado.edu/downloads/76537257b.csv")

# Numeric Data Processing

# Make a copy of the dataset so we don't alter the original dataset; then, view
# the copied dataset 
pt_copy<-pt

# Print contents of "pt_copy"
pt_copy

View(pt_copy)

# Generate summary statistics for "pt_copy" and assign to new object named "pt_copy_summarystats1"
pt_copy_summarystats1<-describe(pt_copy)

# View contents of "pt_copy_summarystats1" in data viewer
View(pt_copy_summarystats1)

# Creates summary statistics for each continent grouping, and puts results in list named "summary_stats_by_continent"
summary_stats_by_continent<-describeBy(pt_copy, pt_copy$continent)

# Accessing continent-level summary statistics for africa from the "summary_stats_by_continent" list
summary_stats_by_continent[["africa"]]

# Group-level summary statistics can be assigned to their own object for easy retrieval
africa_summary<-summary_stats_by_continent[["africa"]]

# Generate a table that displays summary statistics for trade at the continent level and assign to object named "trade_age_by_continent"
trade_age_by_continent<-pt_copy %>% group_by(continent) %>% 
  summarise(meanTrade=mean(trade),sdTrade=sd(trade),
            meanAge=mean(age), sdAge=sd(age),
            n=n())


# prints contents of "trade_age_by_continent"
trade_age_by_continent


# Creates cross-tab showing the breakdown of federal/non federal across continents
crosstab_federal_continent<-pt_copy %>% tabyl(federal, continent)

# Basic data cleaning and preparation tasks

# bring the "country" column to the front of the dataset
pt_copy<-pt_copy %>% relocate(country)

# View "pt_copy"
View(pt_copy)

# bring the "country", "list", "trade", "oecd" columns to the front of the dataset
pt_copy<-pt_copy %>% relocate(country, list, trade, oecd)

View(pt_copy)

## Renaming a variable (renames "list" to "party_list")
pt_copy<-pt_copy %>% rename(party_list=list)

# sorting in ascending (low to high) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(trade)

View(pt_copy)

# sorting in descending (high to low) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(desc(trade))

View(pt_copy)

# Create new variable named "non_catholic_80" that is calculated by substracting the 
# Catholic share of the population in 1980 ("catho80") from 100  and relocates 
# "country", "catho80", and the newly created "non_catholic_80" to the front of the dataset
pt_copy<-pt_copy %>% mutate(non_catholic_80=100-catho80) %>% 
                     relocate(country, catho80, non_catholic_80)

View(pt_copy)


# Selects "country", "cgexp", "cgrev", and "trade" variables from the "pt_copy" 
# dataset and assigns the selection to a new object named "pt_copy_selection"
pt_copy_selection<-pt_copy %>% select(country, cgexp, cgrev, trade, federal)

# deletes "federal" variable from "pt_copy_selection"
pt_copy_selection %>% select(-federal)

# deletes "federal" and "trade" from "pt_copy_selection" and assigns it to new object named "pt_copy_selection_modified"
pt_copy_selection_modified<-pt_copy_selection %>% select(-c(federal, trade))


# Creates a new dummy variable based on the existing "trade" variable named 
# "trade_open" (which takes on a value of "1" if "trade" is greater than or equal to 77, and 0 otherwise) 
# and then moves the newly created variable to the front of the dataset along with 
# "country" and "trade"; all changes are assigned to "pt_copy", thereby overwriting the 
# existing version of "pt_copy"

pt_copy<-pt_copy %>% mutate(trade_open=ifelse(trade>=77, 1, 0)) %>% 
                    relocate(country, trade_open, trade)

View(pt_copy)


# Creates a new variable in the "pt_copy" dataset named "trade_level" (that is coded as 
# "Low Trade" when the "trade" variable is greater than 15 and less than 50, coded as 
# "Intermediate Trade" when "trade" is greater than or equal to 50 and less than 100, 
# and coded as "High TradE" when "trade" is greater than or equal to 100), and then
# reorders the dataset such that "country", "trade_level", and "trade" are the first three 
# variables in the dataset
pt_copy<-pt_copy %>% mutate(trade_level=case_when(trade>15 & trade<50~"Low_Trade",
                                                  trade>=50 & trade<100~"Intermediate_Trade",
                                                  trade>=100~"High_Trade")) %>% 
                      relocate(country, trade_level, trade)


View(pt_copy)


# Creates dummy variables from "trade_level" column, and relocates the new dummies 
# to the front of the dataset
pt_copy<-pt_copy %>% dummy_cols("trade_level") %>% 
                    relocate(country, trade_level, trade_level_High_Trade, trade_level_Intermediate_Trade, trade_level_Low_Trade)



# Extracts OECD observations in "pt_copy" and assigns to object named "oecd_countries"
oecd_countries<-pt_copy %>% filter(oecd==1) %>% 
                            relocate(country, oecd)

View(oecd_countries)


# Extracts observations for which cgrev (central government revenue as % of gdp)>40, 
# and assigns to object named "high_revenues"
high_revenues<-pt_copy %>% filter(cgrev>40) %>% 
                            relocate(country, cgrev)


View(high_revenues)


# Extracts observations for which the "catho80" variable is less than or equal to 50
minority_catholic<-pt_copy %>% filter(catho80<=50) %>% 
                     relocate(country, catho80)


View(minority_catholic)


# Extracts federal OECD countries (where oecd=1 AND federal=1) and assigns to a new 
# object named "oecd_federal_countries"
oecd_federal_countries<-pt_copy %>% filter(oecd==1 & federal==1) %>% 
                                    relocate(country, oecd, federal)



# Extracts observations that are in Africa ("africa") OR in Asia/Europe ("asiae) and 
# assigns to an object named "asia_europe_africa"
asia_europe_africa<-pt_copy %>% filter(continent=="africa"|continent=="asiae") %>% 
                                  relocate(continent)



# Extracts all non-Africa observations and assigns to object named "pt_copy_sans_africa"
pt_copy_sans_africa<-pt_copy %>% filter(continent!="africa") %>% relocate(continent)


# Exploratory Visualizations

# filters Africa observations
pt_africa<-pt_copy %>% 
           filter(continent=="africa")




# Creates a bar chart of the "cgexp" variable (central government expenditure as a share of GDP) 
# for the Africa observations and assigns the plot to an object named "cgexp_africa"
cgexp_africa<-pt_africa %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=country, y=cgexp))+
  labs(
    title="Central Govt Expenditure as Pct of GDP for Select African Countries (1990-1998 Average)",
    x="Country Name", 
    y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90))



# prints contents of cgexp_africa
cgexp_africa

# Creates a bar chart of the "cgexp" variable (central government expenditure as a share of GDP) 
# for the Africa observations; countries are on the x axis and arrayed in ascending order with 
# respect to the cgexp variable, which is on the y-axis; plot is assigned to an object named 
# "cgexp_africa_ascending"
cgexp_africa_ascending<-
  pt_africa %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=reorder(country, cgexp), y=cgexp))+
  labs(
    title="Central Govt Expenditure as Pct of GDP for Select African Countries (1990-1998 Average)",
    x="Country Name", 
    y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90))

# prints contents of "cgexp_africa_ascending"
cgexp_africa_ascending




# Creates a bar chart of the "cgexp" variable (central government expenditure as a share of GDP) 
# for the Africa observations; countries are on the x axis and arrayed in descending order with 
# respect to the cgexp variable, which is on the y-axis; plot is assigned to an object named 
# "cgexp_africa_descending"
cgexp_africa_descending<-
  pt_africa %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_col(aes(x=reorder(country, -cgexp), y=cgexp))+
  labs(
    title="Central Govt Expenditure as Pct of GDP for Select African Countries (1990-1998 Average)",
    x="Country Name", 
    y="CGEXP")+
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90))


# prints "cgexp_africa_descending"
cgexp_africa_descending


# creates a sideways bar chart using the "coord_flip" function and assigns it to a new 
# object named "cgexp_africa_ascending_inverted"  
cgexp_africa_ascending_inverted<-cgexp_africa_ascending+
                                    coord_flip()


# prints "cgexp_africa_ascending_inverted"
cgexp_africa_ascending_inverted


# Creates scatterplot with "cgexp" variable on x-axis and "trade" variiable on 
# y-axis and assigns to object named "scatter_cgexp_trade"
scatter_cgexp_trade<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade))+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 


# prints contents of "scatter_cgexp_trade"
scatter_cgexp_trade


# Creates scatterplot with "cgexp" variable on x-axis and "trade" variable on y-axis, and 
# uses different color points for different continents; plot is assigned to object named 
# "scatter_cgexp_trade_grouped"
scatter_cgexp_trade_grouped<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade, color=continent))+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 


# prints contents of "scatter_cgexp_trade_grouped"
scatter_cgexp_trade_grouped


# Creates continent-level subplots for scatterplot, using facets; assigns plot to new 
# object named "scatter_cgexp_trade_facets"
scatter_cgexp_trade_facets<-
  ggplot(data = pt_copy) + 
  geom_point(mapping = aes(x = cgexp, y = trade)) + 
  facet_wrap(~ continent, nrow = 2)


# prints contents of "scatter_cgexp_trade_facets"
scatter_cgexp_trade_facets


# Creates scatterplot with "cgexp" variable on x-axis and "trade" variiable on y-axis, 
# adds line of best fit; plot assigned to object named "scatter_cgexp_trade_line"
scatter_cgexp_trade_line<-
  pt_copy %>% 
  drop_na(cgexp) %>% 
  ggplot()+
  geom_point(aes(x=cgexp, y=trade))+
  geom_smooth(aes(x=cgexp, y=trade), method="lm")+
  labs(title="Trade Share of GDP \nas a function of\n Central Govt Expenditure (1990-1998 Average) ", 
       x="Central Government Expenditure (Pct of GDP)", y="Overall Trade (Pct of GDP)")+
  theme(plot.title=element_text(hjust=0.5)) 


# Prints contents of "scatter_cgexp_trade_line"
scatter_cgexp_trade_line



# Text Data ---------------------------------------------------------------

# prints ws_df_appended
ws_df_appended

# selects "Abstract" column from "ws_df_appended" and assigns to new object named "wos_abstracts"
wos_abstracts<-ws_df_appended %>% select(Abstract)


# Tokenizes "Abstract" column text by word; assigns tokenized dataset (with words in "word" column) 
# to a new object named "wos_abstracts_tokenized"
wos_abstracts_tokenized<-wos_abstracts %>% 
  unnest_tokens(input=Abstract,
                token="words",
                output=word)


# generates frequency table from "wos_abstracts_tokenized", and assigns 
wos_abstracts_frequency<-wos_abstracts_tokenized %>% 
                          count(word, sort=TRUE)


View(wos_abstracts_frequency)

# prints "stop_words" (part of the "tidytext" package)
stop_words


# cleans "wos_abstracts_frequency" by removing stop words and removing numbers
wos_abstracts_frequency_cleaned<-wos_abstracts_frequency %>% 
                                    filter(!word %in% stop_words$word) %>% 
                                    filter(!grepl('[0-9]', word))  



# creates a new data frame that consists of the rows with the ten highest values 
# for "n" (i.e. the ten most frequently recurring words) and assigns it to a 
# new object named "wos_top_ten"
wos_top_ten<-wos_abstracts_frequency_cleaned %>% 
                slice_max(n, n=10)


# creates bar chart of word frequency data in "wos_top_ten" where words (on the x-axis) are 
# arrayed in ascending order of frequency, and frequency (n) is represented 
# on the Y axis; modified graph is assigned back to "wos_frequency_graph"
wos_frequency_graph<-
  ggplot(data=wos_top_ten)+
  geom_col(aes(x=reorder(word, n), y=n))+
  labs(title="Ten Most Frequent Words in Abstracts of Publications on Climate + Art",
       caption = "Source: Web of Science", 
       x="", 
       y="Frequency")


# prints "wos_frequency_graph" 
wos_frequency_graph



# inverts axes of "wos_frequency_graph" and assigns the result to a new object named "wos_frequency_graph_inverted
wos_frequency_graph_inverted<-wos_frequency_graph+
                                coord_flip()


# prints wos_frequency_graph_inverted
wos_frequency_graph_inverted



# make word cloud based on word frequency information from "wos_abstracts_frequency_cleaned"
wordcloud2(data = wos_abstracts_frequency_cleaned, minRotation = 0, maxRotation = 0, ellipticity = 0.6)


# Automating Data Processing Tasks ----------------------------------------

# write function to take input WOS dataset, select the "Authors", "Article Title", 
# "Source Title", and "Language" columns, rename "Article Title" column to "Article and 
# rename "Source Title" column to "Source", and then subset English language papers;
# the function is assigned to an object named "wos_clean_function"
wos_clean_function<-function(input_dataset){
  modified_dataset<-input_dataset %>% 
    select(Authors, "Article Title", "Source Title", Language) %>% 
    rename("Article"="Article Title",
           "Source"="Source Title") %>% 
    filter(Language=="English")
  return(modified_dataset)
}


# apply "wos_clean_function" to all list elements in "wos_file_list" and assign 
# the new list of modified data frames to a new object named "processed_wos_list"
processed_wos_list<-map(wos_file_list, wos_clean_function)


# print contents of "processed_wos_list"
processed_wos_list


# extract one of the list elements from "processed_wos_list"
processed_wos_list[[3]]


# Exporting Data ----------------------------------------------------------

# Sets working directory to export destination
setwd("~/Documents/git_repositories/R_bootcamp_2023")

# exports pt_africa object to working directory
write_csv(pt_africa, "pt_africa.csv")

# EXPORTING MULTIPLE DATA FRAMES

# Removes the ".csv" suffix from the strings in "wos_files" and then assigns the 
# modified character vector to a new object named "base_names"
base_names<-str_remove(wos_files, ".csv")

# appends the suffix "_modified.csv" to the strings in the "base_names" 
# character vector, and assigns the resulting character vector to a new 
# object named "processed_wos_list_names"
processed_wos_list_names<-paste0(base_names, "_modified.csv")


# prints contents of "processed_wos_list_names"
processed_wos_list_names

# uses the walk2 function to iteratively apply the "write_csv" function, 
# using the data frames in "processed_wos_list" and the file names in
# "processed_wos_list_names" as arguments; the files are written out to the working 
# directory
walk2(processed_wos_list, processed_wos_list_names, write_csv)




# exports "scatter_cgexp_trade_grouped" to working directory as png file
ggsave("africa_bar.png", cgexp_africa_ascending_inverted, width=10, height=5)

# exports "scatter_cgexp_trade_grouped" to working directory as pdf file
ggsave("africa_bar.pdf", cgexp_africa_ascending_inverted, width=10, height=5)

# Uses graphics device to export "scatter_cgexp_trade_grouped" and 
# "cgexp_africa_ascending_inverted" to the working directory as 
# a single PDF file named "workshop_visualizations"
pdf("workshop_visualizations.pdf", width=12, height=5)
scatter_cgexp_trade_grouped
cgexp_africa_ascending_inverted
dev.off()




https://www.dropbox.com/sh/wv6yqtw98umsrto/AACwnspy4wR2ER6jI90YB2dra?dl=0


































