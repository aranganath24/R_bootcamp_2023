## ----setup, include=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=F, warning=F, message=F------------------------------------------------------------------------
library(tidyverse)
library(wosr)
library(DT) 
library(psych)
library(fastDummies)
library(janitor)
library(tidytext)


## ---- echo=FALSE, results='asis', out.width='100%', fig.cap='The RStudio Interface', fig.alt='RStudio Interface Open on Desktop'----
knitr::include_graphics('pictures/rstudio_window_revised.png')


## ---- eval=FALSE------------------------------------------------------------------------------------------
## # Installs "tm" package
## install.packages("tidyverse")


## ---- echo=FALSE, results='asis', out.width='100%', fig.cap='Installing tidyverse in R Script'------------
knitr::include_graphics('pictures/install2.png')


## ---- eval=F----------------------------------------------------------------------------------------------
## install.packages("wosr")


## ---- eval=F----------------------------------------------------------------------------------------------
## # Loads tidyverse packages into memory
## library(tidyverse)


## ---- eval=F----------------------------------------------------------------------------------------------
## # loads remainder of required packages
## library(wosr)
## library(psych)
## library(fastDummies)
## library(janitor)
## library(tidytext)


## ---------------------------------------------------------------------------------------------------------
# calculates 2+2
2+2


## ---------------------------------------------------------------------------------------------------------
# calculates 65 to the power of 4
65^4


## ---------------------------------------------------------------------------------------------------------
# assign value 5 to new object named x
x<-5


## ---------------------------------------------------------------------------------------------------------
# prints value assigned to "x"
x


## ---------------------------------------------------------------------------------------------------------
# assign value 12 to new object named y
y<-12


## ---------------------------------------------------------------------------------------------------------
# prints value assigned to "y"
y


## ---------------------------------------------------------------------------------------------------------
# creates a new object, named "xy_sum" whose value is the sum of "x" and "y"
xy_sum<-x+y


## ---------------------------------------------------------------------------------------------------------
# prints contents of "xy_sum"
xy_sum


## ---------------------------------------------------------------------------------------------------------
# assign value of "8" to object named "x"
x<-8


## ---------------------------------------------------------------------------------------------------------
# prints updated value of "x"
x


## ---------------------------------------------------------------------------------------------------------
xy_sum


## ---------------------------------------------------------------------------------------------------------
# assigns sum of "y" and newly updated value of "x" to "xy_sum" object
xy_sum<-x+y


## ---------------------------------------------------------------------------------------------------------
# prints value of "xy_sum"
xy_sum


## ---------------------------------------------------------------------------------------------------------
our_location<-"Boulder, CO"


## ---------------------------------------------------------------------------------------------------------
# prints value of "our_location" object
our_location


## ---- error=TRUE------------------------------------------------------------------------------------------
our_Location


## ---------------------------------------------------------------------------------------------------------
# makes vector with values 5,7,55,32
c(5, 7, 55, 32)


## ---------------------------------------------------------------------------------------------------------
# assigns vector of arbitrary values to new object named "arbitrary_values"
arbitrary_values<-c(5,7,55.6,32.5)


## ---------------------------------------------------------------------------------------------------------
# prints vector assigned to "arbitrary_values" object
arbitrary_values


## ---------------------------------------------------------------------------------------------------------
# creates a new vector that doubles the values in "arbitrary_values" and assigns it to a new object named 
"arbitrary_values_2x"
arbitrary_values_2x<-arbitrary_values*2

# prints contents of "arbitrary_values_2x"
arbitrary_values_2x


## ---------------------------------------------------------------------------------------------------------
arbitrary_values + arbitrary_values_2x


## ---------------------------------------------------------------------------------------------------------
# extracts third element of "arbitrary_values_2x" vector
arbitrary_values[3]


## ---------------------------------------------------------------------------------------------------------
# extracts a new vector comprised of the 2nd, 3rd, and 4th elements of the existing "arbitrary_values" vector
arbitrary_values[2:4]


## ---------------------------------------------------------------------------------------------------------
# creates character vector whose elements are the first four months of the year, and assigns the vector to a new object named "months_four"
months_four<-c("January", "February", "March", "April")


## ---------------------------------------------------------------------------------------------------------
# prints contents of "months_four"
months_four


## ---------------------------------------------------------------------------------------------------------
# extracts second element of "months_four" object (i.e. the "February" string)
months_four[2]


## ---------------------------------------------------------------------------------------------------------
# subsets the second and third elements of "months_four" object (i.e. the "February" and "March" strings, which are extracted as a new character vector)
months_four[2:3]


## ---------------------------------------------------------------------------------------------------------
# Creates a dummy country-level data frame 
country_df<-data.frame(Country=c("Country A", "Country B", "Country C"),
                       GDP=c(8000, 30000, 23500),
                       Population=c(2000, 5400, 10000),
                       Continent=c("South America", "Europe", "North America"))


## ---------------------------------------------------------------------------------------------------------
# prints "country_df" data frame to console
country_df


## ---------------------------------------------------------------------------------------------------------
# pulls up "country_df" data frame in R Studio data viewer
View(country_df)


## ---- echo=F----------------------------------------------------------------------------------------------
country_df %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# creates list whose elements are the "arbitrary_values" numeric vector, the "months_four" character vector, and the "country_df" data frame, and assigns it to a new object named "example_list"
example_list<-list(arbitrary_values, months_four, country_df)


## ---------------------------------------------------------------------------------------------------------
# prints contents of "example_list"
example_list


## ---------------------------------------------------------------------------------------------------------
# extracts country-level data frame from "example_list"; the country-level data frame is the third element in "example_list"
example_list[[3]]


## ---------------------------------------------------------------------------------------------------------
example_list[c(1,3)]


## ---------------------------------------------------------------------------------------------------------
# creates a character vector of desired names for list elements, and assigns it to a new object named "name_vector"
name_vector<-c("element1", "element2", "element3")


## ---------------------------------------------------------------------------------------------------------
# assigns names from "name_vector" to list elements in "example_list"
names(example_list)<-name_vector


## ---------------------------------------------------------------------------------------------------------
# prints contents of "example_list"
example_list


## ---------------------------------------------------------------------------------------------------------
# Extracts the data frame from "example_list" by its assigned name
example_list[["element3"]]


## ---------------------------------------------------------------------------------------------------------
# # Extracts the "element3" data frame from "example_list" by its index number
example_list[[3]]



## ---------------------------------------------------------------------------------------------------------
# print the data structure of the "example_list" object
class(example_list)


## ---------------------------------------------------------------------------------------------------------
# print the data structure of the "months_four" object
class(months_four)


## ---------------------------------------------------------------------------------------------------------
# print the data structure of the "arbitrary_values" object
class(arbitrary_values)


## ---- eval=F----------------------------------------------------------------------------------------------
## # fahrenheit to Celsius formula, where "F" is fahrenheit input
## C=(F-32)*(5/9)


## ---------------------------------------------------------------------------------------------------------
# Converts 55 degrees fahrenheit to Celsius
(55-32)*(5/9)


## ---------------------------------------------------------------------------------------------------------
# Generates function that takes fahrenheit value ("fahrenheit_input") and returns a value in Celsius, and assigns the function to an object named "fahrenheit_to_celsius_converter"
fahrenheit_to_celsius_converter<-function(fahrenheit_input){
  celsius_output<-(fahrenheit_input-32)*(5/9)
  return(celsius_output)
}


## ---------------------------------------------------------------------------------------------------------
# Uses arithmetic operation to convert 68 degrees Fahrenheit to Celsius
(68-32)*(5/9)


## ---------------------------------------------------------------------------------------------------------
# Uses "fahrenheit_to_celsius_converter" function to convert 68 degrees Fahrenheit to Celsius
fahrenheit_to_celsius_converter(fahrenheit_input=68)


## ---------------------------------------------------------------------------------------------------------
fahrenheit_to_celsius_converter(fahrenheit_input=22)


## ---------------------------------------------------------------------------------------------------------
fahrenheit_to_celsius_converter(fahrenheit_input=45.6)

fahrenheit_to_celsius_converter(fahrenheit_input=95.9)

fahrenheit_to_celsius_converter(fahrenheit_input=67.8)

fahrenheit_to_celsius_converter(fahrenheit_input=43.)


## ---------------------------------------------------------------------------------------------------------
fahrenheit_input_vector<-c(45.6, 95.9, 67.8, 43)


## ---------------------------------------------------------------------------------------------------------
# Iteratively applies the "fahrenheit_to_celsius_converter" to celsius input values in "fahrenheit_input_vector" and assigns the resulting vector of converted temperature values to "celsius_ouputs_vector"
celsius_outputs_vector<-map_dbl(fahrenheit_input_vector, fahrenheit_to_celsius_converter)


## ---------------------------------------------------------------------------------------------------------
# prints contents of "celsius_outputs_vector"
celsius_outputs_vector


## ---------------------------------------------------------------------------------------------------------
# iteratively applies the "fahrenheit_to_celsius_converter" function to the input values in "fahrenheit_input_vector", and assigns the list of celsius output values to a new object named "celsius_outputs_list"
celsius_outputs_list<-map(fahrenheit_input_vector, fahrenheit_to_celsius_converter)


## ---------------------------------------------------------------------------------------------------------
# prints contents of "celsius_outputs_list"
celsius_outputs_list


## ---------------------------------------------------------------------------------------------------------
# checks data structure of "celsius_outputs_list"
class(celsius_outputs_list)


## ---------------------------------------------------------------------------------------------------------
# Creates function that takes an input value in degrees Fahrenheit (fahrenheit_input), converts this value to Celsius, and returns a data frame with the input Fahrenheit temperature value as one column, and the corresponding Celsius temperature value as another column; the function is assigned to a new object named "fahrenheit_to_celsius_converter_df" 
fahrenheit_to_celsius_converter_df<-function(fahrenheit_input){
  celsius_output<-(fahrenheit_input-32)*(5/9)
  celsius_output_df<-data.frame(fahrenheit_input, celsius_output)
  return(celsius_output_df)
}


## ---------------------------------------------------------------------------------------------------------
# applies "fahrenheit_to_celsius_converter_df" function to input value of 63 degrees Fahrenheit
fahrenheit_to_celsius_converter_df(fahrenheit_input=63)


## ---------------------------------------------------------------------------------------------------------
# Iteratively applies the "fahrenheit_to_celsius_converter_df" function to input values in "fahrenheit_input_vector" to generate a data frame with column of input Fahrenheit values, and column of corresponding output Celsius values; assigns this data frame to a new object named "celsius_outputs_df"
celsius_outputs_df<-map_dfr(fahrenheit_input_vector, fahrenheit_to_celsius_converter_df)


## ---------------------------------------------------------------------------------------------------------
# prints contents of 
celsius_outputs_df


## ---------------------------------------------------------------------------------------------------------
?map


## ---------------------------------------------------------------------------------------------------------
# creates sample country-level Fahrenheit data for Country A
countryA_fahrenheit<-c(55,67,91,23, 77, 98, 27)

# creates sample country-level Fahrenheit data for Country B
countryB_fahrenheit<-c(33,45,11,66, 44)

# creates sample country-level Fahrenheit data for Country C
countryC_fahrenheit<-c(60,55,12,109)

# creates sample country-level Fahrenheit data for Country D
countryD_fahrenheit<-c(76, 24, 77, 78)


## ---------------------------------------------------------------------------------------------------------
# Creates list of input vectors and assigns this list to new object named "input_list"
temperature_input_list<-list(countryA_fahrenheit, countryB_fahrenheit, countryC_fahrenheit, countryD_fahrenheit) 


## ---------------------------------------------------------------------------------------------------------
# Iteratively passes vectors in "temperature_input_list" as arguments to "fahrenheit_to_celsius_converter_df" and deposits the resulting data frames to a list, which is assigned to a new object named "processed_temperature_data_list"
processed_temperature_data_list<-map(temperature_input_list, fahrenheit_to_celsius_converter_df)


## ---------------------------------------------------------------------------------------------------------
# prints contents of "processed_temperature_data_list"
processed_temperature_data_list


## ---------------------------------------------------------------------------------------------------------
# Reads in Persson/Tabellini Data from local directory
pt<-read_csv("data/pt/persson_tabellini_workshop.csv")


## ---------------------------------------------------------------------------------------------------------
# print relevant file names
wos_files<-list.files("data/wos")


## ---------------------------------------------------------------------------------------------------------
# prints contents of "wos_files"
wos_files


## ---------------------------------------------------------------------------------------------------------
# Iteratively reads in all individual WOS files from the "data/wos" directory and assigns it to an object named "wos_file_list"
setwd("data/wos")
wos_file_list<-map(wos_files, read_csv)


## ---------------------------------------------------------------------------------------------------------
# appends data frames in "wos_file_list" into one data frame and assigns it to a new object named "ws_df_appended"
ws_df_appended<-bind_rows(wos_file_list)


## ---------------------------------------------------------------------------------------------------------
# Reads in PT dataset from dropbox and assigns it to a new object named "pt_cloud"
pt_cloud<-read_csv("https://www.dropbox.com/s/iczslf52s8bzku2/persson_tabellini_workshop.csv?dl=1")


## ---------------------------------------------------------------------------------------------------------
# Reads in published dataset from CU Scholar and assigns it to a new object named "green_space_CUScholar"
green_space_CUScholar<-read_csv("https://scholar.colorado.edu/downloads/76537257b.csv")


## ---------------------------------------------------------------------------------------------------------
# Make a copy of the dataset so we don't alter the original dataset; then, view
# the copied dataset 
pt_copy<-pt


## ---------------------------------------------------------------------------------------------------------
# Print contents of "pt_copy"
pt_copy


## ---------------------------------------------------------------------------------------------------------
View(pt_copy)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Generate summary statistics for "pt_copy" and assign to new object named "pt_copy_summarystats1"
pt_copy_summarystats1<-describe(pt_copy)


## ---------------------------------------------------------------------------------------------------------
# View contents of "pt_copy_summarystats1" in data viewer
View(pt_copy_summarystats1)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy_summarystats1 %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Creates summary statistics for each continent grouping, and puts results in list named "summary_stats_by_continent"
summary_stats_by_continent<-describeBy(pt_copy, pt_copy$continent)


## ---------------------------------------------------------------------------------------------------------
# Accessing continent-level summary statistics for africa from the "summary_stats_by_continent" list
summary_stats_by_continent[["africa"]]


## ---------------------------------------------------------------------------------------------------------
# Group-level summary statistics can be assigned to their own object for easy retrieval
africa_summary<-summary_stats_by_continent[["africa"]]


## ---------------------------------------------------------------------------------------------------------
# Generate a table that displays summary statistics for trade at the continent level and assign to object named "trade_age_by_continent"
trade_age_by_continent<-pt_copy %>% group_by(continent) %>% 
                                    summarise(meanTrade=mean(trade),sdTrade=sd(trade),
                                              meanAge=mean(age), sdAge=sd(age),
                                              n=n())


## ---------------------------------------------------------------------------------------------------------
# prints contents of "trade_age_by_continent"
trade_age_by_continent


## ---------------------------------------------------------------------------------------------------------
# Creates cross-tab showing the breakdown of federal/non federal across continents
crosstab_federal_continent<-pt_copy %>% tabyl(federal, continent)


## ---------------------------------------------------------------------------------------------------------
# bring the "country" column to the front of the dataset
pt_copy<-pt_copy %>% relocate(country)


## ---------------------------------------------------------------------------------------------------------
# bring the "country", "list", "trade", "oecd" columns to the front of the dataset
pt_copy<-pt_copy %>% relocate(country, list, trade, oecd)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
## Renaming a variable (renames "list" to "party_list")
pt_copy<-pt_copy %>% rename(party_list=list)


## ---------------------------------------------------------------------------------------------------------
# sorting in ascending (low to high) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(trade)


## ---------------------------------------------------------------------------------------------------------
# sorting in descending (high to low) order with respect to the "trade" variable
pt_copy<-pt_copy %>% arrange(desc(trade))


## ---------------------------------------------------------------------------------------------------------
# Create new variable named "non_catholic_80" that is calculated by substracting the Catholic share of the population in 1980 ("catho80") from 100  and relocates "country", "catho80", and the newly created "non_catholic_80" to the front of the dataset
pt_copy<-pt_copy %>% mutate(non_catholic_80=100-catho80) %>% 
                     relocate(country, catho80, non_catholic_80)


## ---- echo=F----------------------------------------------------------------------------------------------
# prints updated contents of "pt_copy"
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Selects "country", "cgexp", "cgrev", and "trade" variables from the "pt_copy" dataset and assigns the selection to a new object named "pt_copy_selection"
pt_copy_selection<-pt_copy %>% select(country, cgexp, cgrev, trade, federal)


## ---- echo=F----------------------------------------------------------------------------------------------
# prints updated contents of "pt_copy"
pt_copy_selection %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# deletes "federal" variable from "pt_copy_selection"
pt_copy_selection %>% select(-federal)


## ---------------------------------------------------------------------------------------------------------
# deletes "federal" and "trade" from "pt_copy_selection" and assigns it to new object named "pt_copy_selection_modified"
pt_copy_selection_modified<-pt_copy_selection %>% select(-c(federal, trade))


## ---- echo=F----------------------------------------------------------------------------------------------
# prints updated contents of "pt_copy"
pt_copy_selection_modified %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Creates a new dummy variable based on the existing "trade" variable named "trade_open" (which takes on a value of "1" if "trade" is greater than or equal to 77, and 0 otherwise) and then moves the newly created variable to the front of the dataset along with "country" and "trade"; all changes are assigned to "pt_copy", thereby overwriting the existing version of "pt_copy"

pt_copy<-pt_copy %>% mutate(trade_open=ifelse(trade>=77, 1, 0)) %>% 
                     relocate(country, trade_open, trade)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Creates a new variable in the "pt_copy" dataset named "trade_level" (that is coded as "Low Trade" when the "trade" variable is greater than 15 and less than 50, coded as "Intermediate Trade" when "trade" is greater than or equal to 50 and less than 100, and coded as "High TradE" when "trade" is greater than or equal to 100), and then reorders the dataset such that "country", "trade_level", and "trade" are the first three variables in the dataset
pt_copy<-pt_copy %>% mutate(trade_level=case_when(trade>15 & trade<50~"Low_Trade",
                                                  trade>=50 & trade<100~"Intermediate_Trade",
                                                  trade>=100~"High_Trade")) %>% 
                    relocate(country, trade_level, trade)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---- message=FALSE---------------------------------------------------------------------------------------
# Creates dummy variables from "trade_level" column, and relocates the new dummies to the front of the dataset
pt_copy<-pt_copy %>% dummy_cols("trade_level") %>% 
                      relocate(country, trade_level, trade_level_High_Trade, trade_level_Intermediate_Trade, trade_level_Low_Trade)


## ---- echo=F----------------------------------------------------------------------------------------------
pt_copy %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# Extracts OECD observations in "pt_copy" and assigns to object named "oecd_countries"
oecd_countries<-pt_copy %>% filter(oecd==1) %>% 
                            relocate(country, oecd)


## ---- echo=F----------------------------------------------------------------------------------------------
oecd_countries %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
# prints ws_df_appended
ws_df_appended


## ---------------------------------------------------------------------------------------------------------
wos_abstracts<-ws_df_appended %>% select(Abstract)


## ---------------------------------------------------------------------------------------------------------
wos_abstracts_tokenized<-wos_abstracts %>% 
                          unnest_tokens(input=Abstract,
                                        token="words",
                                        output=word)


## ---------------------------------------------------------------------------------------------------------
wos_abstracts_frequency<-wos_abstracts_tokenized %>% 
                          count(word, sort=TRUE)


## ---------------------------------------------------------------------------------------------------------
stop_words


## ---------------------------------------------------------------------------------------------------------
wos_abstracts_frequency_cleaned<-wos_abstracts_frequency %>% 
                                    filter(!word %in% stop_words$word) %>% 
                                    filter(!grepl('[0-9]', word))   


## ---------------------------------------------------------------------------------------------------------
wos_top_ten<-wos_abstracts_frequency_cleaned %>% 
              slice_max(n, n=10)


## ---------------------------------------------------------------------------------------------------------
wos_top_ten %>% datatable(extensions=c("Scroller", "FixedColumns"), options = list(
  deferRender = TRUE,
  scrollY = 350,
  scrollX = 350,
  dom = "t",
  scroller = TRUE,
  fixedColumns = list(leftColumns = 3)
))


## ---------------------------------------------------------------------------------------------------------
wos_frequency_graph<-
  ggplot(data=wos_top_ten)+
    geom_col(aes(x=word, y=n))+
    labs(title="Ten Most Frequent Words in Abstracts of Publications on Climate + Art",
         caption = "Source: Web of Science", 
         x="", 
         y="Frequency")


## ---------------------------------------------------------------------------------------------------------
wos_frequency_graph<-
  ggplot(data=wos_top_ten)+
   geom_col(aes(x=reorder(word, n), y=n))+
    labs(title="Ten Most Frequent Words in Abstracts of Publications on Climate + Art",
         caption = "Source: Web of Science", 
         x="", 
         y="Frequency")


## ---------------------------------------------------------------------------------------------------------
wos_frequency_graph_inverted<-
    ggplot(data=wos_top_ten)+
   geom_col(aes(x=reorder(word, n), y=n))+
   coord_flip()+
    labs(title="Ten Most Frequent Words in Abstracts of Publications on Climate + Art",
         caption = "Source: Web of Science", 
         x="", 
         y="Frequency")

