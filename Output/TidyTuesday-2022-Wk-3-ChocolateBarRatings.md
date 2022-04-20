TidyTuesday - Chocolate Bar Ratings
================
Philip Wong
2022-04-19

<strong> Week 3 - TidyTuesday Challenge: </strong> <br> The data this
week comes from the from ***Flavors of Cacao*** by way of ***Georgios
and Kelsey***. <br> Project Source:
<https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-18/readme.md>
<br>

<strong> Table of Contents: </strong> <br>

1.  Importing packages & datasets
2.  Data exploration (# of records, NA values, variable types)
3.  Data cleaning & manipulation
4.  Data visualizations that’ll answer the following questions;
    -   What’s the distribution of coffee bean rating’s?

    -   What’s the distribution of cocoa percentages?

    -   Are coffee bean rating’s increasing each year?

    -   Which countries of bean origin shows the highest average rating?

    -   Are there any significant correlation among all numerical
        variables?

### \[1\] Import Packages & Datasets

``` r
library(tidyverse)
library(scales)
library(reshape2)
library(ggcorrplot) # to build correlation heatmap
library(knitr)
library(plotly)

knitr::opts_chunk$set(cache = TRUE, 
                      warning = FALSE,
                      message = FALSE, 
                      echo = TRUE, 
                      fig.width = 7, 
                      fig.height = 6)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

babyblue <- '#89CFF0'
```

### \[2\] Data Exploration

##### 2.2 - Dataframe summary

``` r
# let's view the dataframe size & datatypes of each variable
glimpse(chocolate)
```

    ## Rows: 2,530
    ## Columns: 10
    ## $ ref                              <dbl> 2454, 2458, 2454, 2542, 2546, 2546, 2…
    ## $ company_manufacturer             <chr> "5150", "5150", "5150", "5150", "5150…
    ## $ company_location                 <chr> "U.S.A.", "U.S.A.", "U.S.A.", "U.S.A.…
    ## $ review_date                      <dbl> 2019, 2019, 2019, 2021, 2021, 2021, 2…
    ## $ country_of_bean_origin           <chr> "Tanzania", "Dominican Republic", "Ma…
    ## $ specific_bean_origin_or_bar_name <chr> "Kokoa Kamili, batch 1", "Zorzal, bat…
    ## $ cocoa_percent                    <chr> "76%", "76%", "76%", "68%", "72%", "8…
    ## $ ingredients                      <chr> "3- B,S,C", "3- B,S,C", "3- B,S,C", "…
    ## $ most_memorable_characteristics   <chr> "rich cocoa, fatty, bready", "cocoa, …
    ## $ rating                           <dbl> 3.25, 3.50, 3.75, 3.00, 3.00, 3.25, 3…

##### 2.2 - Managing NULL values

Are there any NA values worth investigating within our dataframe?

``` r
colSums(is.na(chocolate))
```

    ##                              ref             company_manufacturer 
    ##                                0                                0 
    ##                 company_location                      review_date 
    ##                                0                                0 
    ##           country_of_bean_origin specific_bean_origin_or_bar_name 
    ##                                0                                0 
    ##                    cocoa_percent                      ingredients 
    ##                                0                               87 
    ##   most_memorable_characteristics                           rating 
    ##                                0                                0

It appears that our ‘ingredients’ column contains a total of 87 NULL
values.

### \[3\] Data Cleaning & Manipulation

##### 3.1 - Drop columns & rows with NA values

``` r
chocolate_df <- chocolate %>% select(-ref)
chocolate_df <- chocolate_df %>% drop_na()
head(chocolate_df)
```

    ## # A tibble: 6 × 9
    ##   company_manufacturer company_location review_date country_of_bean_origin
    ##   <chr>                <chr>                  <dbl> <chr>                 
    ## 1 5150                 U.S.A.                  2019 Tanzania              
    ## 2 5150                 U.S.A.                  2019 Dominican Republic    
    ## 3 5150                 U.S.A.                  2019 Madagascar            
    ## 4 5150                 U.S.A.                  2021 Fiji                  
    ## 5 5150                 U.S.A.                  2021 Venezuela             
    ## 6 5150                 U.S.A.                  2021 Uganda                
    ## # … with 5 more variables: specific_bean_origin_or_bar_name <chr>,
    ## #   cocoa_percent <chr>, ingredients <chr>,
    ## #   most_memorable_characteristics <chr>, rating <dbl>

##### 3.2 - Transform the datatypes of any quantifiable variables to numeric

We will convert the ***cocoa_percent*** variable to numeric, while
creating a new column that specifies the volume of ingredients by
parsing the ***ingredients*** variable (a string datatype).

``` r
chocolate_df <- chocolate_df %>% 
  mutate(cocoa_percent = round(as.numeric(str_replace_all(cocoa_percent, "%", ""))/100, 2)) %>% 
  separate(ingredients, c("ingredients_volume", "second_column"), sep = "-", remove = FALSE) %>% 
  mutate(ingredients_volume = as.numeric(ingredients_volume)) %>% 
  select(-second_column)

head(chocolate_df)
```

    ## # A tibble: 6 × 10
    ##   company_manufacturer company_location review_date country_of_bean_origin
    ##   <chr>                <chr>                  <dbl> <chr>                 
    ## 1 5150                 U.S.A.                  2019 Tanzania              
    ## 2 5150                 U.S.A.                  2019 Dominican Republic    
    ## 3 5150                 U.S.A.                  2019 Madagascar            
    ## 4 5150                 U.S.A.                  2021 Fiji                  
    ## 5 5150                 U.S.A.                  2021 Venezuela             
    ## 6 5150                 U.S.A.                  2021 Uganda                
    ## # … with 6 more variables: specific_bean_origin_or_bar_name <chr>,
    ## #   cocoa_percent <dbl>, ingredients <chr>, ingredients_volume <dbl>,
    ## #   most_memorable_characteristics <chr>, rating <dbl>

### \[4\] Data Visualizations

##### 4.1 - What’s the distribution of coffee bean rating’s?

``` r
chocolate_df %>% 
  mutate(rating = round(rating, 1)) %>% 
  ggplot(aes(x = rating)) + 
  geom_histogram(bins = 12, alpha = 0.8, fill = babyblue) +
  theme_bw() +
  labs(y = 'Frequency', title = "Most coffee beans contain high rating's between 3-4%")
```

![](/Users/philipwong/Documents/Analytics%20Projects/TidyTuesday/Output/TidyTuesday-2022-Wk-3-ChocolateBarRatings_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
##### 4.2 - What’s the distribution of cocoa percentages?

``` r
chocolate_df %>% 
  mutate(cocoa_percent = round(cocoa_percent, 2)) %>% 
  ggplot(aes(x = cocoa_percent)) +
  geom_histogram(bins = 15, alpha = 0.6, fill = "orange") +
  theme_bw() +
  labs(y = 'Frequency', title = "Majority of cocoa percentages are within 70-80%")
```

![](/Users/philipwong/Documents/Analytics%20Projects/TidyTuesday/Output/TidyTuesday-2022-Wk-3-ChocolateBarRatings_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
##### 4.3 - Are coffee bean rating’s increasing each year?

``` r
chocolate_df %>% 
  ggplot(aes(x = review_date, y = rating, color = cocoa_percent)) +
  geom_point() +
  geom_jitter(alpha = 0.5) + # helps with overplotting - one or more points in the same place
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(title = "There is an increasing trend of coffee bean ratings!")
```

![](/Users/philipwong/Documents/Analytics%20Projects/TidyTuesday/Output/TidyTuesday-2022-Wk-3-ChocolateBarRatings_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

##### 4.4 - Which countries of bean origin shows the highest average rating?

``` r
# to capture the top 20 countries based on sum of rating's
top15_avgRating <- chocolate_df %>% 
  group_by(country_of_bean_origin) %>% 
  summarise(
    sumRating = sum(rating),
    reviewCount = n()) %>% 
  ungroup() %>%
  filter(reviewCount > 20) %>% 
  arrange(desc(sumRating)) %>% 
  top_n(20, sumRating)

chocolate_df %>% 
  filter(country_of_bean_origin %in% top15_avgRating$country_of_bean_origin) %>%
  group_by(country_of_bean_origin) %>% 
  mutate(avgRating = round(mean(rating),1)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(country_of_bean_origin, avgRating), y = rating, fill = avgRating)) +
  geom_boxplot(alpha = 0.8) +
  coord_flip() +
  theme_bw() +
  labs(x = 'Coffee Bean - Origin Country', title = "Vietnam takes the lead based on average rating's")
```

![](/Users/philipwong/Documents/Analytics%20Projects/TidyTuesday/Output/TidyTuesday-2022-Wk-3-ChocolateBarRatings_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
##### 4.5 - Are there any significant correlations?

``` r
correlation_df <- chocolate_df %>% 
  group_by(company_manufacturer, review_date) %>% 
  mutate(distinct_products = n_distinct(specific_bean_origin_or_bar_name)) %>% # number of distinct products for each company per year
  ungroup() %>% 
  select_if(is.numeric) # selecting only numerical variables

corr <- round(cor(correlation_df),1)

ggcorrplot(corr, 
 outline.col = 'white', 
 lab = TRUE,
 ggtheme = ggplot2::theme_gray,
 colors = c("red", babyblue, "royal blue"),
 title = "There are no significant correlations")
```

![](/Users/philipwong/Documents/Analytics%20Projects/TidyTuesday/Output/TidyTuesday-2022-Wk-3-ChocolateBarRatings_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
