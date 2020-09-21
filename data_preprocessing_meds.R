library(dplyr)
library(stringr)
library(reshape2)
library(tidyverse)
library(pastecs)
library(nortest)

# Importing the dataset
dataset = read.csv('/Users/Leeza A. Santos/OneDrive/MS AHI/HHA 507/data_meds_ph.csv')

# Create a sub-sample of the dataset since it is large / random selection of 20 rows
temp = sample_n(dataset, 20)

# Preview the set, see how it looks 
temp 

# Looks like there is a erraneous column 1 - lets remove 
dataset = select(dataset, -c(1))

# Now confirm that it is gone: 
colnames(dataset)

# Update are working temp dataset
temp = sample_n(dataset, 20)

# Lets now confirm the column types in our dataset
# the below function uses sapply, which means we want to apply a function to all columns/rows in 
# our dataframe, and the argument we are using is 'class', which means we want to understand the 
# class of each of the columns in our dataframe called 'dataset'
sapply(dataset, class)
# so it looks like our two dates, created and date_expired have already been processed
# as being dates, which is = POSIXct / POSIXt 
# so unlike pandas/python script, we do not need to do anything to our dates

# It appears 'name' represents drug name, lets rename the column so it is more clear
# in the line below, we say that we want to rename our 'name' column into 'drug_name'
# notice that unlike python, we 1) can use <- which is the equivalent of our = command, 
# and 2) that when we are renaming our column, we are not using quotations 
dataset <- dataset %>% rename(drug_name = name) 

# lets confirm the change has happened: 
temp = sample_n(dataset, 20)
temp
# great! looks better 

# Lets now also do the same for 'roa' which stands for route_of_administration 
# here now below, you can see that R can still take = command, which is the same thing as <- 
dataset = dataset %>% rename(route_of_admin = roa) 

# for 'frequency' column, it looks like there is the medical abbreviation, 
# and then there is the actual human-readable form - lets make this one column 
# two columns - one with the abbreviation, and a seperate one with the full description 
# in the first line below, we make sure that the column is a string 
# in the second line below, we then see that the abbreviation and seperated by some white space, 
# so in our code, we first give the names of the two new columns that we want to create, which is 
# "frequency_abbreviation" and "frequency_readible", then we SPLIT the columns by the white space, 
# which is designated by " " 
newColNames <- c("frequency_abbreviation", "frequency_readible")
newCols <- colsplit(dataset$frequency, " ", newColNames)
dataset <- cbind(dataset, newCols)

#confirm
colnames(dataset)

#remove our old 'frequency' column
dataset = subset(dataset, select = -c(frequency) )

# now lets confirm...
temp = sample_n(dataset, 20)
temp

# now, get some of the drug details OUT of the drug_details column 
# first, remove the white space using str_trim and then we will search for 
# starting and stopping words, extract those to seperates lists, then merge them back together
# into our dataframe

dataset$drug_details = str_trim(dataset$drug_details, side = c("both", "left", "right"))

medispan_id <- dataset$drug_details %>% str_extract("(?<='medispan_id':).*(?='name')")
ndcndf_id <- dataset$drug_details %>% str_extract("(?<='ndcndf_id':).*(?='product_name')")
product_name <- dataset$drug_details %>% str_extract("(?<='product_name':).*(?='va_id')")

drug_details_detail <- data.frame(medispan_id=medispan_id, ndcndf_id=ndcndf_id, product_name=product_name )

clean_df <- cbind(drug_details_detail, dataset)

#confirm
temp = sample_n(clean_df, 20)
temp

#missing data
missing <- data.frame(sapply(clean_df, function(x) sum(is.na(x))))

#how many unique patients we have in this DF 
sum(!duplicated(clean_df$UUID))

#how the average number of TOTAL medications for each patient 
counts = data.frame(clean_df %>% group_by(UUID) %>% summarize(count=n()))
mean(counts$count)

#find out the frequency counts for 'added_by' --> each number represents a unique physician 
added_by_counts = data.frame(table(clean_df$addedby))

#commonly prescribed medication is by 'product_name'
common_meds = data.frame(table(clean_df$product_name))

#commonly prescribed medication is by 'drug_name'
common_drug_name = data.frame(table(clean_df$drug_name))

#which medications have the highest re-fill rate 
refilled = data.frame(clean_df %>% 
  group_by(refills_rxed, product_name) %>%
  summarise(number = n()))

#which medications are the most prescribed by doses_rxed column
doses = data.frame(clean_df %>% 
                        group_by(doses_rxed, product_name) %>%
                        summarise(number = n()))


#what the most common route of administration is 
roa = data.frame(table(clean_df$route_of_admin))
# some repeat values in ROA

#look at some visualizations and means to understand if our continuous data is NORMAL 
# the only variables that are continuous that we are interested in here are doses_rxed and refills_rxed

# lets first get the mean, median, and mode + quartiles for doses_rxed
# we first need to make sure the column is not a string, but a integer 
clean_df$doses_rxed = as.numeric(as.character(clean_df$doses_rxed))
doses_rxed_table = summary(clean_df$doses_rxed)
# and here is another way that uses a additional package called "pastecs" 
# install.packages("pastecs")
doses_rxed_table_stats <- stat.desc(clean_df$doses_rxed)
doses_rxed_table_stats <- data.frame(round(doses_rxed_table_stats, 2))


#get the mean, median, and mode + quartiles for refills_rxed
# we first need to make sure the column is not a string, but a integer 
clean_df$refills_rxed = as.numeric(as.character(clean_df$refills_rxed))
refills_rxed_table = summary(clean_df$refills_rxed)
#another way: 
refills_rxed_table_stats <- stat.desc(clean_df$refills_rxed)
refills_rxed_table_stats <- data.frame(round(refills_rxed_table_stats, 2))

#find the mean, median, mode, SD for doses_rxed to determine normality 
#first changed to integer
clean_df$doses_rxed = as.numeric(as.character(clean_df$doses_rxed))
doses_rxed_table = summary(clean_df$doses_rxed)
doses_rxed_table_stats <- stat.desc(clean_df$doses_rxed)
doses_rxed_table_stats <- data.frame(round(doses_rxed_table_stats, 2))

#another normality test, Shapiro-Wilks test, n = 5000
clean_df$doses_rxed = as.numeric(as.character(clean_df$doses_rxed))
doses_rxed = clean_df$doses_rxed
doses_rxed_small = head(doses_rxed, n=5000)
shapiro.test(doses_rxed_small)

clean_df$refills_rxed = as.numeric(as.character(clean_df$refills_rxed))
refills_rxed = clean_df$refills_rxed
refills_rxed_small = head(refills_rxed, n=5000)
shapiro.test(refills_rxed_small)

#Anderson Darling test, normality 'doses_rxed' and 'refills_rxed'
ad.test(clean_df$doses_rxed)
ad.test(clean_df$refills_rxed)


# box plots 
boxplot(clean_df$refills_rxed)
boxplot(clean_df$doses_rxed)

# histogram
hist(clean_df$refills_rxed)
hist(clean_df$doses_rxed)


