# Install requested packages if not found
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(tidyr))
  install.packages("tidyr")
# Load libraries
library(tidyverse)
library(caret)
library(hexbin)
library(lubridate)
library(knitr)
library(kableExtra)
# Adult Census Income dataset
# http://archive.ics.uci.edu/ml/machine-learning-databases/adult
datafile <- "Income.RData"
# Check if datafile is already downloaded
if (!file.exists("Income.RData"))
{
  fileURL <-
    "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
  
  destfile_adult <- "./dataset/adult.data"
  
  # Download only if destination data file - adult.data
  # is not found on the dataset directory folder
  if (!file.exists(destfile_adult))
  {
    # Create dataset folder file if it doesn't exists
    if (!dir.exists("./dataset"))
    {
      dir.create("./dataset")
    }
    
    # Download file from url to temp file
    download.file(fileURL, destfile_adult,  quiet = TRUE)
    print("Downloading file to the dataset directory")
    
  }
  
  c_names <-
    c(
      "age", "workclass", "fnlwgt", "education", "education-num", 
      "marital-status", "occupation", "relationship", "race", "sex", 
      "capital-gain", "capital-loss", "hours-per-week", "native-country", "income"
    )
  
  
  # read the data from the file and store in the census_data object
  census_data <-
    read.csv(file = "./dataset/adult.data", header = FALSE, 
             col.names = c_names, stringsAsFactors = T, strip.white = TRUE)
  
  # replace . in the columns names
  colnames(census_data) <- str_replace_all(colnames(census_data), "\\.", "_")
  
  # Save census_data object to datafile
  save(census_data, file = datafile)
  
  # Remove unused objects from the memory
  rm(c_names, fileURL, datafile, destfile_adult)
  
} else {
  # Load the datafile if it already exists
  load(datafile)
}


census_data$workclass<-ifelse(census_data$workclass=='?',
                              NA, as.character(census_data$workclass))
census_data$occupation<-ifelse(census_data$occupation=='?',
                               NA, as.character(census_data$occupation))
census_data$native_country<-ifelse(census_data$native_country=='?',
                                   NA, as.character(census_data$native_country))


census_data <- na.omit(census_data)
nrow(census_data)

census_data <- census_data %>%
  select(-education_num)

# to combine capital gain and capital loss columns
# into a single column Capital,
# a positive value represents gain and negative represents loss
census_data<- census_data %>%
  mutate(capital = capital_gain - capital_loss) %>%
  # then remove capital_gain and capital_loss
  select(-capital_gain,-capital_loss)

census_data <- census_data %>%
  mutate(income = factor(if_else(income == "<=50K", 0, 1)))

census_data <- census_data %>%
  select(-fnlwgt)



census_data$marital_status <-
  as.character(census_data$marital_status)

census_data$marital_status[census_data$marital_status == "Married-AF-spouse" |
                             census_data$marital_status == "Married-civ-spouse" |
                             census_data$marital_status == "Married-spouse-absent"] <-
  "Married"

census_data$marital_status[census_data$marital_status == "Divorced" |
                             census_data$marital_status == "Separated" |
                             census_data$marital_status == "Widowed"] <-
  "Not-married"

table(census_data$marital_status)


# to combine the column values catagories 

LatinAmerica <- c("Dominican-Republic","Guatemala","Haiti","Honduras",
                  "Jamaica","Mexico","Nicaragua", "Outlying-US(Guam-USVI-etc)", 
                  "Puerto-Rico","Trinadad&Tobago","Cuba")

SouthAmerica <- c("Peru","Ecuador","El-Salvador","Columbia")

Europe <- c("France","Germany","Greece","Holand-Netherlands",
            "Hungary","Italy","Poland", "Portugal","Puerto-Rico",
            "South","Ireland","Yugoslavia")

Asia <- c("China","Hong","India","Japan","Iran")

SE_Aisa <- c("Vietnam","Cambodia","Thailand","Laos","Philippines","Taiwan")

US <- c("United-States")
UK <- c("England","Scotland")
Canada <- c("Canada")




census_data$native_country[census_data$native_country %in% LatinAmerica] <-
  "Latin America"
census_data$native_country[census_data$native_country %in% Asia] <-
  "Asia"
census_data$native_country[census_data$native_country %in% SE_Aisa] <-
  "South East Aisa"
census_data$native_country[census_data$native_country %in% SouthAmerica] <-
  "South America"
census_data$native_country[census_data$native_country %in% Europe] <-
  "Europe"
census_data$native_country[census_data$native_country %in% US] <-
  "US"
census_data$native_country[census_data$native_country %in% UK] <-
  "UK"
census_data$native_country[census_data$native_country %in% Canada] <-
  "Canada"



table(census_data$native_country) %>%
  kable() %>%
  kable_styling(latex_options = c("striped", "hover", "condensed"))






census_data$education = gsub("^10th", "No-college", census_data$education)
census_data$education = gsub("^11th", "No-college", census_data$education)
census_data$education = gsub("^12th", "No-college", census_data$education)
census_data$education = gsub("^1st-4th", "No-college", census_data$education)
census_data$education = gsub("^5th-6th", "No-college", census_data$education)
census_data$education = gsub("^7th-8th", "No-college", census_data$education)
census_data$education = gsub("^9th", "No-college", census_data$education)
census_data$education = gsub("^Assoc-acdm", "Associates", census_data$education)
census_data$education = gsub("^Assoc-voc", "Associates", census_data$education)
census_data$education = gsub("^Bachelors", "Bachelors", census_data$education)
census_data$education = gsub("^Doctorate", "Doctorate", census_data$education)
census_data$education = gsub("^HS-Grad", "HS-Graduate", census_data$education)
census_data$education = gsub("^Masters", "Masters", census_data$education)
census_data$education = gsub("^Preschool", "No-college", census_data$education)
census_data$education = gsub("^Prof-school", "Prof-School", census_data$education)
census_data$education = gsub("^Some-college", "Some-college", census_data$education)

# to display the data in a tabular form
table(census_data$education) %>%
  kable() %>%
  kable_styling(latex_options = c("striped", "hover", "condensed"))





census_data$workclass = gsub("^Without-pay", "Not-Working", census_data$workclass)
census_data$workclass = gsub("^Never-worked", "Not-Working", census_data$workclass)
census_data$workclass = gsub("^Self-emp-not-inc", "Self-Employed", census_data$workclass)
census_data$workclass = gsub("^Self-emp-inc", "Self-Employed", census_data$workclass)

# to display the data in a tabular form
table(census_data$workclass) %>%
  kable() %>%
  kable_styling(latex_options = c("striped", "hover", "condensed"))