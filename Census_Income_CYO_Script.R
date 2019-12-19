##########################################################################################################################
# Script for Data Science Capstone CYO Adult Census Income Project (HarvardX PH125.9x)
# Author: Ravi Jha
# Date: 18 Dec 2019
# github: https://github.com/jha-r/Harvardx-PH125.9x-Capstone-Census_Income_CYO
##########################################################################################################################

##########################################################################################################################
#### Summary: 
# This script is a part of project, prepared to fulfill the completion requirement of 
# the HavardX PH125.9x Data Science Capstone course. This script is consistent with the Census_Income_CYO.Rmd

# Following are the Key code blocks in this script

# Importing the dataset and setup
# Data wrangling
# Exploratory data analysis and visualization 
# Spliting the curated dataset and processing
# Creating and tuning predective models
# Reporting results 

##########################################################################################################################

###### START OF SCRIPT ###########


############################################################################################################
#### Data Import and Initial Setup ####
## Comments: This block consists of code to import and setup Adult Census Income dataset
## http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
############################################################################################################

    # Install requested packages if not found
    if (!require(tidyverse))
      install.packages("tidyverse", repos = "http://cran.us.r-project.org")
    if (!require(caret))
      install.packages("caret", repos = "http://cran.us.r-project.org")
    if (!require(data.table))
      install.packages("data.table", repos = "http://cran.us.r-project.org")
    if (!require(e1071))
      install.packages("e1071", dependencies=TRUE, 
                       repos = "http://cran.us.r-project.org")
    if (!require(rpart))
      install.packages("rpart", repos = "http://cran.us.r-project.org")
    if (!require(ranger))
      install.packages("ranger", dependencies=TRUE, 
                       repos = "http://cran.us.r-project.org")
    if (!require(dummies))
      install.packages("dummies", repos = "http://cran.us.r-project.org")
    if (!require(dplyr))
      install.packages("dplyr")
    if (!require(tidyr))
      install.packages("tidyr")    
    
    # Load libraries
    library(tidyverse) # to untidy data
    library(caret) # Classification And Regression
    library(e1071) # SVM 
    library(ggplot2) # for plotting graphs
    library(dummies) # dummy variables (one hot encoder)
    library(ranger) # fast Random Forest
    library(rpart) # Decision Tree
    library(class) # KNN
    library(hexbin) # plotting
    library(lubridate) # date
    library(knitr) # LaTeX pdf
    library(kableExtra) # LaTeX pdf

    # Adult Census Income dataset
    # http://archive.ics.uci.edu/ml/machine-learning-databases/adult

    # to store objects from memory to the filesystem
    datafile <- "Income.RData"
    
    # to check if datafile is already downloaded
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
      # to name dataset columns
      c_names <-
        c("age", "workclass", "fnlwgt", "education", "education-num", 
          "marital-status", "occupation", "relationship", "race", "sex", 
          "capital-gain", "capital-loss", "hours-per-week", "native-country", "income")
      
      
      # to read the data from the file and store in the census_data object
      census_data <-
        read.csv(file = "./dataset/adult.data", header = FALSE, 
                 col.names = c_names, stringsAsFactors = T, strip.white = TRUE)
      
      # replace . in the columns names
      colnames(census_data) <- str_replace_all(colnames(census_data), "\\.", "_")
      
      # to Save census_data object to datafile
      save(census_data, file = datafile)
      
      # Remove unused objects from the memory
      rm(c_names, fileURL, datafile, destfile_adult)
      
      } else {
        # lo Load the datafile if it already exists
        load(datafile)
      }

    
##########################################################################################################
#### Data Wrangling ####
# Comments:
# This block contains code for a brief data review and data wrangling as per the necessity.
##########################################################################################################    

#####################################
### 3.1 Initial exploration
#####################################
    
    # Display first few rows to understand the data structure
    glimpse(census_data)
    # The dataset has 32,561 records and 15 attributes.


#### Dataset Summary 
    # Summary of the dataset
    summary(census_data)


#### Column types and class
    # To display dataset column types and summary
    col_typ <- as.data.frame(lapply(census_data, class)) %>%
      .[1, ] %>%
      gather(variable, class , 1:ncol(.)) %>%
      mutate(data_type = ifelse(
        class == "factor",
        "categorical",
        ifelse(variable == "education_num", "categorical", "continuous")
      )) 
    col_typ %>%
      # to format table with theme
      kable() %>%
      kable_styling(latex_options = c("striped", "hover", "condensed"))  %>%
      row_spec(0, bold = T)


##################################################
### 3.2 Tidying data
##################################################
    
#### Duplicate Records

# to check for duplicate records
    census_data %>%
      summarize(record_count = n(),
                distinct_records = n_distinct(.)) %>%
      mutate(duplicate_records = record_count - distinct_records) %>%
      mutate(duplicate_percent = 
               paste0(round(duplicate_records / record_count * 100, 1), " %")) %>%
      # to format table with theme
      kable() %>%
      kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
      row_spec(0, bold = T)




#### Missing Data
    
#to check if any explicit missing value marked with NA
      anyNA(census_data)
      # to check missing values labeled as "?"
      # first check for the records with any missing value (?)
      missing_count_tbl <-
        map_df(census_data, ~ str_detect(., pattern = "\\?")) %>%
        rowSums() %>%
        tbl_df() %>%
        filter(value > 0) %>%
        summarize(missing_count = n()) %>%
        # to create a table to display the missing count and percent
        mutate(missing_percent =
                 paste0(round(missing_count / nrow(census_data) * 100, 1), "%"),
               Column = "Record with any missing columns") %>% 
        
        select(Column, missing_count, missing_percent)
      
      # now check for individual columns with missing values
      # and bind it with the existing table
      
      map_df(census_data, ~ sum(str_detect(., pattern = "\\?"))) %>%
        
        gather(Column, missing_count, 1:15) %>%
        mutate(missing_percent = 
                 paste0(round(missing_count / nrow(census_data) * 100, 1), "%")) %>%
        bind_rows(missing_count_tbl) %>%
        # filter-out columns with no missing value
        filter(missing_count > 1) %>%
        # arrange rows by missing count
        arrange(desc(missing_count)) %>%
        # to format table with theme
        kable() %>%
        kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
        row_spec(0, bold = T) 



#### Impute missing values

      # first to convert missing values ? to NA
      # for workclass, occupation, and native_country
      census_data$workclass<-ifelse(census_data$workclass=='?',
                                    NA, as.character(census_data$workclass))
      census_data$occupation<-ifelse(census_data$occupation=='?',
                                     NA, as.character(census_data$occupation))
      census_data$native_country<-ifelse(census_data$native_country=='?',
                                         NA, as.character(census_data$native_country))
      
      # next remove rows with missing values
      census_data <- na.omit(census_data)



#### Combine Marital Status column catagories

      # to combine the column values catagories 
      census_data$marital_status <- as.character(census_data$marital_status)
      
      # to create two new categories, Never-married is already present
      Married <- c("Married-AF-spouse", "Married-civ-spouse", "Married-spouse-absent")
      Notmarried <- c("Divorced","Separated", "Widowed")
      
      # to update the marital_status column with the new catagories    
      census_data$marital_status[census_data$marital_status %in% Married] <-
        "Married"
      census_data$marital_status[census_data$marital_status %in% Notmarried] <-
        "Not-married"
      
      # to display the table
      census_data %>%
        group_by(marital_status) %>%
        summarise(count =n(), 
                  proportion = paste0(round(count/nrow(census_data)*100, digits = 2),"%")) %>%
        arrange(desc(count)) %>%
        kable() %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))  %>%
        row_spec(0, bold = T)


#### Remove Education_Num column

      # to remove education_num column
      census_data <- census_data %>%
        select(-education_num)


#### Combine Capital Gain and Capital Loss columns

      # to combine capital gain and capital loss columns
      # into a single column Capital,
      # a positive value represents gain and negative represents loss
      census_data <- census_data %>%
        mutate(capital = capital_gain - capital_loss) %>%
        # then remove capital_gain and capital_loss
        select(-capital_gain, -capital_loss)


#### Remove Final Weight column

      # to remove fnlwgt column
      census_data <- census_data %>%
        select(-fnlwgt)


#### Combine Native Country column catagories

      # to combine the column values catagories 
      
      # following are the new catogories
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
      
      # to update the native_country column with the new catagories    
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
      
      # to display the data in a tabular form
      census_data %>%
        group_by(native_country) %>%
        summarise(count =n(), 
                  proportion = paste0(round(count/nrow(census_data)*100, digits = 2),"%")) %>%
        arrange(desc(count)) %>%
        kable() %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))  %>%
        row_spec(0, bold = T)


  
#### Combine Education column catagories

      # to combine the column values catagories 
      
      # No-college and Associates are the new additonal categories
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
      census_data %>%
        group_by(education) %>%
        summarise(count =n(), 
                  proportion = paste0(round(count/nrow(census_data)*100, digits = 2),"%")) %>%
        arrange(desc(count)) %>%        
        kable() %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))  %>%
        row_spec(0, bold = T)


      
#### Combine Workclass column categories 

      # to combine the column values catagories 
      # Not-Working and Self-Employed are two additional categories
      census_data$workclass = gsub("^Without-pay", "Not-Working", census_data$workclass)
      census_data$workclass = gsub("^Never-worked", "Not-Working", census_data$workclass)
      census_data$workclass = gsub("^Self-emp-not-inc", "Self-Employed", census_data$workclass)
      census_data$workclass = gsub("^Self-emp-inc", "Self-Employed", census_data$workclass)
      
      # to display the data in a tabular form
      census_data %>%
        group_by(workclass) %>%
        summarise(count =n(), 
                  proportion = paste0(round(count/nrow(census_data)*100, digits = 2),"%")) %>%
        arrange(desc(count)) %>%        
        kable() %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))  %>%
        row_spec(0, bold = T)



#######################################################################################
####  Exploratory Data Analysis and Visualization ####
# Comments:
# The block of code is to perform exploratory data analysis on the tidy data by utilizing visualization techniques.
#######################################################################################

#### Age vs income
      
      # to plot density graph for age and income
      ggplot(census_data, aes(x = age, color = income, fill = income)) +
        geom_density(alpha = 0.8) +
        # to provide a title, x, and y axis labels
        labs(x = "Age", y = "Density",
             title = "Age vs income desity graph"
        )
      
      # to plot a table with median age for both income categories
      census_data %>%
        group_by(income) %>%
        summarise(Median_age = median(age)) %>%
        kable("latex", booktabs = T) %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))



#### Native Country vs income
      
      # to plot native_country vs income
      
      # first create a plot order as per native_country bar lengths
      plot_order <-
        reorder(census_data$native_country, census_data$native_country, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +
        # to provide a title, x, and y axis labels      
        labs(x = "Native Country", y = "Count", 
             title = "Impact of the country of origin on income classification")
      
      # to display category proportion as per income distribution
      round(prop.table(table(census_data$native_country, 
                             census_data$income), 1) * 100, digits = 2)




#### Occupation vs income
      
      # to plot occupation vs income
      
      # first create a plot order as per occupation bar lengths
      plot_order <-
        reorder(census_data$occupation, census_data$occupation, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +
        # to provide a title, x, and y axis labels      
        labs(x = "Occupation", y = "Count", 
             title = "Biased in the income based on occupation")
      
      # to display category proportion as per income distribution
      round(prop.table(table(census_data$occupation, 
                             census_data$income), 1) * 100, digits = 2)   
      
      
      
#### Education vs income
      
      # to plot education vs income
      
      # first create a plot order as per education bar lengths
      plot_order <-
        reorder(census_data$education, census_data$education, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +
        # to provide a title, x, and y axis labels
        labs(x = "Education Level", y = "Count", 
             title = "Education bias")
      
      # to display category proportion as per income distribution    
      round(prop.table(table(census_data$education, 
                             census_data$income), 1) * 100, digits = 2)     
      
      
      
      
#### Workclass vs income
      
      # to plot workclass vs income
      
      # first create a plot order as per workclass bar lengths
      plot_order <-
        reorder(census_data$workclass, census_data$workclass, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +  
        # to provide a title, x, and y axis labels
        labs(x = "Working Class", y = "Count",
             title = "Working class bias")
      
      # to display category proportion as per income distribution 
      round(prop.table(table(census_data$workclass, 
                             census_data$income), 1) * 100, digits = 2)     
      
      
      
#### Marital Status vs income
      
      # to plot marital_status vs income
      
      # first create a plot order as per marital_status bar lengths
      plot_order <-
        reorder(census_data$marital_status, census_data$marital_status, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +
        # to provide a title, x, and y axis labels      
        labs(x = "Marital Status", y = "Count", 
             title = "Marital Status vs income")
      
      # to display category proportion as per income distribution    
      round(prop.table(table(census_data$marital_status, 
                             census_data$income), 1) * 100, digits = 2)      
      
      
      
#### Race vs income
      
      # to plot race vs income
      
      # first create a plot order as per race bar lengths
      plot_order <-
        reorder(census_data$race, census_data$race, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text
        coord_flip() +
        # to provide a title, x, and y axis labels      
        labs(x = "Race", y = "Count", 
             title = "Race bias")
      
      # to display category proportion as per income distribution
      round(prop.table(table(census_data$race, 
                             census_data$income), 1) * 100, digits = 2)      
      
      
      
#### Gender vs income
      
      # to plot sex vs income
      
      # first create a plot order as per sex bar lengths
      plot_order <-
        reorder(census_data$sex, census_data$sex, length)
      plot_order <-
        factor(plot_order, levels = rev(levels(plot_order)))
      # plot grap using ggplot    
      ggplot(census_data, aes(plot_order)) +
        geom_bar(aes(fill = income), color = "#1380A1", alpha = 0.8) +
        # to flip coordinate to adust the text      
        coord_flip() +
        # to provide a title, x, and y axis labels      
        labs(x = "Gender", y = "Count", title = "Men tends to earn more")
      
      # to display category proportion as per income distribution
      round(prop.table(table(census_data$sex, 
                             census_data$income), 1) * 100, digits = 2)
      
      
      
#### Working hours vs income
      
      # plot grap using ggplot
      ggplot(census_data, aes(x = hours_per_week, fill = income, color = income)) +
        geom_bar(alpha = 0.8, position = "fill") +
        # to flip coordinate to adust the text  
        coord_flip() +
        # to provide a title, x, and y axis labels  
        labs(x = "Hours per Week", y = "Proportion", title = "Hours/week vs Proportion")
      
      # to plot a table with mean working hours for both income categories
      census_data %>%
        group_by(income) %>%
        summarise(hours = mean(hours_per_week)) %>%
        kable("latex", booktabs = T) %>%
        kable_styling(latex_options = c("striped", "hover", "condensed"))
      
      
      
      
#### Outlier Data points
      
#### Outliers: Education level
      
      census_data %>%
        # to map age and education level variables to aesthetics of ggplot function
        ggplot(aes(age, education)) +
        # to plot scattered data using geom_point
        geom_point( alpha = 0.3, col = "#00AFBB") +
        # to highlight obscure outlier points with different color
        geom_point(aes(col = (age<= 20 & education == 'Masters')), alpha = 1) +
        scale_colour_manual(values = setNames(c('red','#00AFBB'),c(T, F)))+
        # to move legend at the end of the figure
        theme(legend.position="bottom") +
        # to scale the axis values
        scale_x_continuous(breaks = seq(0,100,10))+
        # to provide a title, x, and y axis labels
        xlab("Age") + 
        ylab("Education Level") +
        ggtitle("Age vs Education Level")
      
      
      #These obscure data points can be removed using the code below.
      census_data <- census_data %>%
        filter(!(age <= 20 & education == 'Masters'))
        
      
#### Outliers: Sex and Relationship
      census_data %>%
        # to map sex and relationship variables to aesthetics of ggplot function
        ggplot(aes(sex, relationship)) +
        # to plot scattered data using geom_point
        geom_point( alpha = 0.5, col = "#00AFBB") +
        # to highlight obscure outlier points with different color
        geom_point(aes(col = (sex == 'Male' & relationship == 'Wife') 
                       | (sex == 'Female' & relationship == 'Husband')), alpha = 1)+
        scale_colour_manual(values = setNames(c('red','#00AFBB'),c(T, F)))+
        # to move legend at the end of the figure  
        theme(legend.position="bottom") +
        
        # to provide a title, x, and y axis labels
        xlab("Sex") + 
        ylab("Relationship") +
        ggtitle("Gender vs Relationship")
      
      #These obscure data points can be removed using the code below.
      census_data <- census_data %>%
        filter(!((sex == 'Male' & relationship == 'Wife') 
                 | (sex == 'Female' & relationship == 'Husband')))
       
      
      
#### Outliers: Hours per week
      census_data %>%
        # to map age and hours per week variables to aesthetics of ggplot function
        ggplot(aes(age, hours_per_week)) +
        # to plot scattered data using geom_point
        geom_point( alpha = 0.3, col = "#00AFBB") +
        # to highlight obscure outlier points with different color  
        geom_point(aes(col = (age >= 80 & hours_per_week >40)),alpha = 1)+
        scale_colour_manual(values = setNames(c('red','#00AFBB'),c(T, F)))+
        # to scale the axis values
        scale_x_continuous(breaks = seq(0,100,10))+
        scale_y_continuous(breaks = seq(0,100,10))+
        # to move legend at the end of the figure  
        theme(legend.position="bottom") +
        
        # to provide a title, x, and y axis labels
        xlab("Age") + 
        ylab("Hours per week") +
        ggtitle("Age vs Hours per week")
      
      #These obscure data points can be removed using the code below.
      census_data <- census_data %>%
        filter(!(age >= 80 & hours_per_week >40))
       
      
      
#### Outliers: Capital
      census_data %>%
        # to map age and capital variables to aesthetics of ggplot function
        ggplot(aes(age, capital)) +
        
        # to plot scattered data using geom_point
        geom_point(alpha = 0.3, col = "#00AFBB") +
        # to highlight obscure outlier points with different color   
        geom_point(aes(col = (capital >= 99999)), alpha = 1)+
        scale_colour_manual(values = setNames(c('red','#00AFBB'),c(T, F)))+
        # to scale the axis values  
        scale_x_continuous(breaks = seq(0,100,10))+
        # to move legend at the end of the figure  
        theme(legend.position="bottom") +
        
        # to provide a title, x, and y axis labels
        xlab("Age") + 
        ylab("Capital") +
        ggtitle("Age vs Capital")
      
      #These obscure data points can be removed using the code below.
      census_data <- census_data %>%
        filter(!(capital >= 99999))
      
      
##################################################  
### Dataset processing and splitting
##################################################
      
#### Factorizing
      
      # to check the current state of the data
      glimpse(census_data)
      
      # to factoring variables to exclude the unwanted levels
      census_data$workclass <- factor(census_data$workclass)
      census_data$occupation <- factor(census_data$occupation)
      census_data$native_country <- factor(census_data$native_country)
      census_data$education <- factor(census_data$education)
      census_data$marital_status <- factor(census_data$marital_status)
      
      # let's check if any NA generated due to cleanup activity.
      anyNA(census_data)



#### Encoding: One-Hot Encode Factors

    # to factoring variables to exclude the unwanted levels
    census_data <- dummy.data.frame(census_data)
    # to check the impact of the encoding
    glimpse(census_data)

 


#### Combine Income columns

    # to keep only a single column called income 
    census_data <- census_data %>%
      mutate(income = census_data$'income>50K')
    
    # then remove census_data$'income>50K' and census_data$'income<=50K'
    census_data <-
      census_data[, -which(names(census_data) %in% c('income>50K', 'income<=50K'))]



##########################################################################################################
#### Spliting dataset: Training and Validation Sets ####
## Comments:
# In this code block, the dataset will first be split into cd_sub and cd_validation set (20%), 
# and then the cd_sub set will then be further split into a cd_train and cd_test

##########################################################################################################

 ##############################################################################
 # to split datasets into cd_sub, cd_validation, cd_train, and cd_test
 ##############################################################################
       set.seed(1)
       # Partition the data set into cd_sub and cd_validation dataset.
       # The cd_test set will be 20% of census_data

       # to create the index with 80% train and 20% test
       indextemp <-
         sample(1:nrow(census_data), size = as.integer(nrow(census_data) * 0.8))
      
       cd_sub <- census_data[indextemp,]
       cd_validation  <- census_data[-indextemp,]
      
       #  # Now, further split the cd_sub into cd_train and cd_test dataset
       # with respect to dependent variable income
       index <-
         sample(1:nrow(cd_sub), size = as.integer(nrow(cd_sub) * 0.8))
      
       cd_train <- cd_sub[index,]
       cd_test  <- cd_sub[-index,]
      
      
      
       #### Labels Dataset
       #extract 'income' column of dataset to labels datasets
       cd_train_labels <- cd_train$income
       cd_test_labels <- cd_test$income
      
       # then remove income label from the train and test dataset
       cd_train <- cd_train %>%
         select(-income)
       cd_test  <- cd_test %>%
         select(-income)
       # to check the dimensions of the datasets
       dim(cd_train)
       dim(cd_test)
      
      
      
#### Feature Scaling
      
       # to scale features between 0 and 1.
       preprocessor <-
         preProcess(cd_train, method = 'range', rangeBounds = c(0, 1))
       cd_train <- predict(preprocessor, cd_train)
       cd_test <- predict(preprocessor, cd_test)
      
       # to check the dataset after scaling
       glimpse(cd_train)


 #### Principal Component Analysis (PCA) to reduce dimensionality

       # to perform PCA on train and test datasets with threshold of 95% variance
       preprocessor <- preProcess(cd_train, method = 'pca', threshold = 0.95)
       cd_train <- predict(preprocessor, cd_train)
       cd_test <- predict(preprocessor, cd_test)
       # to check the dataset after performing PCA
       glimpse(cd_train)


# to combine labels in the train and test datasets after performing PCA

       cd_train_all <-
         cbind(cd_train, cd_train_labels)
       cd_test_all <-
         cbind(cd_test, cd_test_labels)

 
 
 #######################################################################################
 #### 5. Modeling Approach  ####
 # Comments: This section of code modeling approacha and insights gained from them.
 #######################################################################################

##################################################
### 5.2 Model 1: Logistic Regression Model
##################################################
 
      # to create logistic regression model
      log_model <- glm(cd_train_labels ~ ., data = cd_train, family = binomial(link='logit'))
     
      #to loop through various threshold to find the optimal threshold
      thresholds <- seq(0, 1, 0.05)
  
      Prediction <- predict(log_model, newdata = cd_test, type = "response")
      # loop function using sapply
      ACCs <- sapply(thresholds, function(threshold) {
          pred_table <- table(cd_test_labels, Prediction >= threshold)
        # return the calculated accuracy
        return((sum(diag(pred_table)) / sum(pred_table)))
      })
      
      # to plot threshold point vs accuracy
      qplot(thresholds, ACCs, geom = c("point", "line"))
      
      # to find the optimal threshhold for which accuracy is maximum
      threshold_log <- thresholds[which.max(ACCs)]
      
      threshold_log
      # accuracy for the optimal threshhold value
      log_acc <- max(ACCs)


# Accuracy_results
      # to display Model Accuracy results in a tabular form
      model_results <- tibble(Model = "Logistic Regression Model", 
                             Dataset = "cd_test", 
                             Accuracy = round(log_acc * 100, digits = 2))
      
      model_results %>% 
      # to apply theme to the table
      kable("latex", booktabs = T) %>% 
      kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
      row_spec(0, bold = T) %>%
      # to highlight the last row
      row_spec(1:1, bold = T, color = "white", background = "#D7261E")



##################################################
### 5.3 Model 2: Decision Tree Model
##################################################
      
      # to create Decision Tree Model model
      dt_model <-
        train(cd_train_labels ~ ., data = cd_train_all, method = "rpart")
  
      # to predict using test dataset
      Prediction_dt <-
        predict(dt_model, newdata = cd_test)
      # to calculate model accuracy
      dt_acc <-
        mean(round(as.numeric(Prediction_dt)) == cd_test_labels)
  
      # append results to the table
      model_results <- bind_rows(model_results,
                                  tibble(Model = "Decision Tree Model",
                                         Dataset = "cd_test",
                                         Accuracy = round(dt_acc * 100,
                                                          digits = 2)))
      model_results %>%
            kable("latex", booktabs = T)  %>%
            kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
            row_spec(0, bold = T) %>%
            # to highlight the last row
            row_spec(2:2, bold = T, color = "white", background = "#D7261E")



    
##################################################
### 5.4 Model 3: Random Forest
##################################################
      
      #to loop through various threshold to find the optimal threshold (tree no.)
      trees <- seq(50, 150, 10)
  
      ACCs <- sapply(trees, function(tree) {
      # to create Random Forest model
      rf_model <-
        ranger(cd_train_labels ~ ., data = cd_train_all, num.trees = tree)
  
      Prediction_rf <-
        predict(rf_model, cd_test_all)
      # to calculate model accuracy
       return( mean(round(as.numeric(Prediction_rf$predictions)) == cd_test_labels))
      })
      
      # to plot threshold point vs accuracy
      qplot(trees, ACCs, geom = c("point", "line"))
      
      # to find the optimal threshhold (no. of trees) for which accuracy is maximum
      tree_rf <- trees[which.max(ACCs)]
      tree_rf
      # accuracy for the optimal threshhold value
      rf_acc <- max(ACCs)
  
      # append results to the table
      model_results <- bind_rows(model_results,
                                  tibble(Model = "Ramdom Forest Model",
                                         Dataset = "cd_test",
                                         Accuracy = round(rf_acc * 100,
                                                          digits = 2)))
      model_results %>%
            kable("latex", booktabs = T)  %>%
            kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
            row_spec(0, bold = T) %>%
            # to highlight the last row
            row_spec(3:3, bold = T, color = "white", background = "#D7261E")



##################################################
### 5.5 Model 4: Support Vector Machine
##################################################
    
      # to create Support Vector Machine model
  
      # loop through costs to find optimal cost for svm
      costs = c(1, 3, 5, 7, 15)   
      ACCs <- sapply(costs, function(cost) {
        svm_model <- svm(cd_train_labels ~ ., data = cd_train, cost = cost)
        pred <- predict(svm_model, cd_test)
        
        # to calculate model accuracy
        return(mean(round(as.numeric(pred)) == cd_test_labels))
      })
      
      # to plot threshold point vs accuracy
      qplot(costs, ACCs, geom = c("point", "line"))
      
      # to find the optimal cost for which accuracy is maximum
      cost_svm <- costs[which.max(ACCs)]
      cost_svm
      # accuracy for the optimal cost value
      svm_acc <- max(ACCs)
  
      # append results to the table
      model_results <- bind_rows(model_results,
                                  tibble(Model = "Support Vector Machine",
                                         Dataset = "cd_test",
                                         Accuracy = round(svm_acc * 100,
                                                          digits = 2)))
      model_results %>%
            kable("latex", booktabs = T)  %>%
            kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
            row_spec(0, bold = T) %>%
            # to highlight the last row
            row_spec(4:4, bold = T, color = "white", background = "#D7261E")
    
  

##################################################
### 5.6 Model 5: k Nearest Neighbors
##################################################
      
      # to create KNN model
      #to loop through various threshold to find the optimal threshold
      thresholds <- seq(5, 25, 1)
      
      ACCs <- sapply(thresholds, function(threshold) {
        knn_model <- knn(cd_train, cd_test,
                         cl = cd_train_labels, k = threshold)
        # return the calculated accuracy      
        return(mean(knn_model == cd_test_labels))
      })
  
      # to plot threshold point vs accuracy
      qplot(thresholds, ACCs, geom = c("point", "line"))
      
      # to find the optimal threshhold for which accuracy is maximum
      threshold <- thresholds[which.max(ACCs)]
      
      threshold
      # accuracy for the optimal threshhold value
      knn_acc <- max(ACCs)
  
      # append results to the table
      model_results <- bind_rows(model_results,
                                  tibble(Model = "k Nearest Neighbor",
                                         Dataset = "cd_test",
                                         Accuracy = round(knn_acc * 100,
                                                          digits = 2)))
      model_results %>%
            kable("latex", booktabs = T)  %>%
            kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
            row_spec(0, bold = T) %>%
            # to highlight the last row
            row_spec(5:5, bold = T, color = "white", background = "#D7261E")
      



###  5.7 Evaluating the selected model on validation dataset

 ############################################################################
 # Predict using Logistic Regression Model on validation set
 ############################################################################
    
     #extract 'income' column of dataset to labels datasets
     cd_train_vlabels <- cd_sub$income
     cd_test_vlabels <- cd_validation$income
    
     # then remove income label from the cd_sub and validation dataset
    cd_sub <- cd_sub %>%
       select(-income)
     cd_validation  <- cd_validation  %>%
       select(-income)
    
     # to scale features between 0 and 1.
     preprocessor <-
       preProcess(cd_sub, method = 'range', rangeBounds = c(0, 1))
     cd_sub <- predict(preprocessor, cd_sub)
     cd_validation <- predict(preprocessor, cd_validation)
    
     # to perform PCA on cd_sub and validation datasets with threshold of 95% variance
     preprocessor <- preProcess(cd_sub, method = 'pca', threshold = 0.95)
     cd_sub <- predict(preprocessor, cd_sub)
     cd_validation <- predict(preprocessor, cd_validation)
    
     # optimal threshold selected in Log Regression Model
     threshold_log
     # to create logistic regression model using validation set
     log_model_val <- glm(cd_train_vlabels ~ ., data = cd_sub, family = binomial(link='logit'))
    
     Prediction <- predict(log_model_val, newdata = cd_validation, type = "response")
     pred_table <- table(cd_test_vlabels, Prediction >= threshold_log)
    
     # accuracy for the optimal threshhold value
     log_val_acc <- (sum(diag(pred_table)) / sum(pred_table))
    
     # append results to the table
     model_results <- bind_rows(model_results,
                                tibble(Model = "Logistic Regression Model",
                                       Dataset = "cd_validation",
                                       Accuracy = round(log_val_acc * 100,
                                                        digits = 2)))
     model_results %>%
       # to apply theme to the table
       kable("latex", booktabs = T) %>%
       kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
       row_spec(0, bold = T) %>%
       # to highlight the last row
       row_spec(6:6,bold = T, color = "white", background = "#D7261E")
    
     
     
 
#######################################################################################
#### Modeling Results  ####
# Comments: Modeling result comparison and performance
#
#######################################################################################
  
  # to display result tables with prediction model results 
      model_results %>% 
            kable("latex", booktabs = T)  %>% 
            kable_styling(latex_options = c("striped", "hover", "condensed")) %>%
            row_spec(0, bold = T) %>%
            # to highlight the last row
            row_spec(6:6, bold = T, color = "white", background = "#3DDB48")
  


# memory cleanup
     # Remove objects
     rm(Married, Notmarried, LatinAmerica, SouthAmerica, Europe, Asia, SE_Aisa, US, UK, Canada)
     # Call Garbage Collector
     gc()

