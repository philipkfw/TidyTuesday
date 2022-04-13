TidyTuesday - Dog Breeds!
================
Philip Wong
2022-04-12

<strong> Week 5 - TidyTuesday Challenge: </strong> <br> The data this
week comes from the ***American Kennel Club*** courtesy of KKakey -
thanks! <br> Project Source:
<https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md>
<br>

<strong> Table of Contents: </strong> <br>

1.  Importing packages & datasets
2.  Data exploration (# of records, NA values, variable types)
3.  Data cleaning & manipulation
4.  Data visualizations that’ll answer the following questions;
    -   Based on the ranking’s provided by AKC registration statistics -
        what do the top 5 breeds have in common?

    -   Any particular correlation that help potential dog owners select
        their breeds?

### \[1\] Import Packages & Datasets

``` r
library(tidyverse)
library(scales)
```

    ## Warning: package 'scales' was built under R version 4.1.3

``` r
library(reshape2)
library(ggcorrplot) # to build correlation heatmap
```

    ## Warning: package 'ggcorrplot' was built under R version 4.1.3

``` r
library(knitr)

knitr::opts_chunk$set(cache = TRUE, warning = FALSE,
                      message = FALSE, echo = TRUE, dpi = 180,
                      fig.width = 7, fig.height = 5)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')
```

### \[2\] Data Exploration

##### 2.1 - Managing duplicates

Within the ***breeds_traits*** dataframe, can we verify if each record
contains a distinct breed? Are there any duplicate breed records?

``` r
n_row <- nrow(breed_traits) # number of records
ndistinct_breeds <- n_distinct(breed_traits$Breed) # number of distinct breeds
if (n_row == ndistinct_breeds) {"Dataframe contains distinct breeds"} else {"Dataframe contains duplicate breeds"}
```

    ## [1] "Dataframe contains distinct breeds"

##### 2.2 - Managing NULL values

Are there any NA values worth investigating within our 3 dataframes?

``` r
colSums(is.na(breed_traits)) # no NA values
```

    ##                      Breed   Affectionate With Family 
    ##                          0                          0 
    ##   Good With Young Children       Good With Other Dogs 
    ##                          0                          0 
    ##             Shedding Level    Coat Grooming Frequency 
    ##                          0                          0 
    ##             Drooling Level                  Coat Type 
    ##                          0                          0 
    ##                Coat Length      Openness To Strangers 
    ##                          0                          0 
    ##          Playfulness Level Watchdog/Protective Nature 
    ##                          0                          0 
    ##         Adaptability Level         Trainability Level 
    ##                          0                          0 
    ##               Energy Level              Barking Level 
    ##                          0                          0 
    ##   Mental Stimulation Needs 
    ##                          0

``` r
colSums(is.na(breed_rank_all)) # contains NA values
```

    ##     Breed 2013 Rank 2014 Rank 2015 Rank 2016 Rank 2017 Rank 2018 Rank 2019 Rank 
    ##         0        19        19        19        19        19        19        20 
    ## 2020 Rank     links     Image 
    ##         0         0         0

### \[3\] Data Cleaning & Manipulation

##### 3.1 - Join dataframes

Left join ‘breed_rank_all’ dataframe onto ‘breed_traits’ dataframe
(primary table)

``` r
# before joining - we need to remove any brackets as dplyr's left_join will not be able to detect brackets within a string
breed_traits <- breed_traits %>% mutate(Breed = str_replace_all(Breed, "[//(//)]", "")) %>% mutate(Breed = str_squish(Breed))
breed_rank_all <- breed_rank_all %>% mutate(Breed = str_replace_all(Breed, "[//(//)]", "")) %>% mutate(Breed = str_squish(Breed)) 

# replace spaces with an underscore (Breed column)
breed_traits$Breed <- str_replace_all(breed_traits$Breed, " ", "_")
breed_rank_all$Breed <- str_replace_all(breed_rank_all$Breed, " ", "_")

# left join by 'Breed' column
joined_df <- breed_traits %>% left_join(breed_rank_all, by=c('Breed'))
head(joined_df)
```

    ## # A tibble: 6 x 27
    ##   Breed      `Affectionate ~` `Good With You~` `Good With Oth~` `Shedding Level`
    ##   <chr>                 <dbl>            <dbl>            <dbl>            <dbl>
    ## 1 Retriever~                5                5                5                4
    ## 2 French_Bu~                5                5                4                3
    ## 3 German_Sh~                5                5                3                4
    ## 4 Retriever~                5                5                5                4
    ## 5 Bulldogs                  4                3                3                3
    ## 6 Poodles                   5                5                3                1
    ## # ... with 22 more variables: `Coat Grooming Frequency` <dbl>,
    ## #   `Drooling Level` <dbl>, `Coat Type` <chr>, `Coat Length` <chr>,
    ## #   `Openness To Strangers` <dbl>, `Playfulness Level` <dbl>,
    ## #   `Watchdog/Protective Nature` <dbl>, `Adaptability Level` <dbl>,
    ## #   `Trainability Level` <dbl>, `Energy Level` <dbl>, `Barking Level` <dbl>,
    ## #   `Mental Stimulation Needs` <dbl>, `2013 Rank` <dbl>, `2014 Rank` <dbl>,
    ## #   `2015 Rank` <dbl>, `2016 Rank` <dbl>, `2017 Rank` <dbl>, ...

##### 3.2 - Drop columns & rows with NA values

``` r
joined_df <- joined_df %>% select(-c(links, Image))
joined_df <- joined_df %>% drop_na()
head(joined_df)
```

    ## # A tibble: 6 x 25
    ##   Breed      `Affectionate ~` `Good With You~` `Good With Oth~` `Shedding Level`
    ##   <chr>                 <dbl>            <dbl>            <dbl>            <dbl>
    ## 1 Retriever~                5                5                5                4
    ## 2 French_Bu~                5                5                4                3
    ## 3 German_Sh~                5                5                3                4
    ## 4 Retriever~                5                5                5                4
    ## 5 Bulldogs                  4                3                3                3
    ## 6 Poodles                   5                5                3                1
    ## # ... with 20 more variables: `Coat Grooming Frequency` <dbl>,
    ## #   `Drooling Level` <dbl>, `Coat Type` <chr>, `Coat Length` <chr>,
    ## #   `Openness To Strangers` <dbl>, `Playfulness Level` <dbl>,
    ## #   `Watchdog/Protective Nature` <dbl>, `Adaptability Level` <dbl>,
    ## #   `Trainability Level` <dbl>, `Energy Level` <dbl>, `Barking Level` <dbl>,
    ## #   `Mental Stimulation Needs` <dbl>, `2013 Rank` <dbl>, `2014 Rank` <dbl>,
    ## #   `2015 Rank` <dbl>, `2016 Rank` <dbl>, `2017 Rank` <dbl>, ...

##### 3.3 - Replace column header’s spaces with underscore

``` r
names(joined_df) <- str_replace_all(names(joined_df), " ", "_")
```

### \[4\] Data visualizations

##### 4.1 - What do top 5 breeds have in common?

``` r
melt_vector1 <- as.vector(c("Breed", "2013_Rank", "2014_Rank", "2015_Rank", "2016_Rank",
                          "2017_Rank", "2018_Rank", "2019_Rank", "2020_Rank", "Coat_Type", "Coat_Length"))

melt_df <- melt(joined_df, melt_vector1, variable.name = "Trait", value.name = "Trait_Score")
melt_df <- melt(melt_df, 
                c("Breed", "Coat_Type", "Coat_Length", "Trait", "Trait_Score"), 
                variable.name = "Year", 
                value.name = "Rank")

# manipulate 'Year' column
melt_df <- melt_df %>% 
  mutate(Year = str_replace(Year, "_Rank", "")) %>% 
  mutate(Year = as.numeric(Year))

# build plot
melt_df %>% 
  filter(Rank <= 5) %>% 
  filter(Trait_Score == 5) %>% 
  group_by(Trait) %>%
  summarise(Total_Trait_Score = sum(Trait_Score)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(Trait, Total_Trait_Score), y = Total_Trait_Score)) +
  geom_bar(stat = 'identity', fill = 'royal blue') +
  coord_flip() +
  theme_bw() +
  labs(x = "Breed Traits", y = "Total Score Across Top 5 Breeds")
```

![](C:\Users\philwong\DOCUME~1\ANALYT~1\R\TIDYTU~1\Output\TIDYTU~1/figure-gfm/unnamed-chunk-7-1.PNG)<!-- -->

##### 4.2 - Any particular correlation that help potential dog owners select their breeds?

``` r
correlation_df <- joined_df %>% 
  select_if(is.numeric) %>% 
  select(!ends_with("_Rank"))

corr <- round(cor(correlation_df),1)

ggcorrplot(corr, 
           outline.col = 'white', 
           lab = TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("red", "grey", "royal blue"))
```

![](C:\Users\philwong\DOCUME~1\ANALYT~1\R\TIDYTU~1\Output\TIDYTU~1/figure-gfm/unnamed-chunk-8-1.PNG)<!-- -->
