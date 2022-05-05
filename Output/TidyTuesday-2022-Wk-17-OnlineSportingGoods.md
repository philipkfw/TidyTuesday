TidyTuesday-2022-Wk-17-OnlineSportingGoods
================
Philip Wong
2022-05-04

<strong> Week 17 - TidyTuesday Challenge: </strong> <br> This week we
will be analyzing a ***product-related*** dataset from Kaggle as opposed
to the datasets provided in TidyTuesday’s GitHub. <br>  
***Project Source:***
<https://www.kaggle.com/datasets/podsyp/how-to-do-product-analytics>
<br>

***Project Scenario*** <br> Online store of sporting goods: clothing,
shoes, accessories and sports nutrition. <br>

On the main page of the store’s website, users will view banners in
order to stimulate their sales. Each user will be able to see one of 5
banners, randomly selected and displayed. Each banner either advertises
a specific product or the entire company. We believe the experience with
banners can vary by segment, and their effectiveness may depend on the
characteristics of user behavior.” <br>

***Problem:*** <br> Let’s use our analytical skills to drive decisions around sales/user growth! We’re looking to answer the following questions. <br> 
- Which banner contains the highest number of returning visitors?  <br>
- Which days of the week results in the highest average user traffic (clicks & impressions)? <br> 
- What time of day results in the highest average user traffic, given same timezone? <br> 
- Which banner drove the most sales? <br> 
- For each banner, what’s the average timespan before users make their first purchase? <br>

### \[1\] Import Packages & Datasets

``` r
library(tidyverse)
library(data.table)
library(scales)
library(reshape2)
library(ggcorrplot) # to build correlation heatmap
library(knitr)
library(lubridate)
library(plotly)
library(zoo)

knitr::opts_chunk$set(cache = TRUE, 
                      warning = FALSE,
                      message = FALSE, 
                      echo = TRUE, 
                      fig.width = 7, 
                      fig.height = 6)

product <- read_csv("../Data/TTWK17_product.csv")

theme_blue <- "#2E15DA"
theme_grey <- "#858286" 
theme_purple <- "#A714E7"
```

### \[2\] Data exploration & pre-processing

##### 2.1 - Dataframe summary

***Data Description:*** <br>

``` r
# let's view the dataframe size & datatypes of each variable
glimpse(product)
```

    ## Rows: 8,471,220
    ## Columns: 8
    ## $ order_id     <chr> "cfcd208495d565ef66e7dff9f98764da", "c4ca4238a0b923820dcc…
    ## $ user_id      <chr> "c81e728d9d4c2f636f067f89cc14862c", "eccbc87e4b5ce2fe2830…
    ## $ page_id      <chr> "6f4922f45568161a8cdf4ad2299f6d23", "4e732ced3463d06de0ca…
    ## $ product      <chr> "sneakers", "sneakers", "sports_nutrition", "company", "c…
    ## $ site_version <chr> "desktop", "desktop", "desktop", "desktop", "desktop", "m…
    ## $ time         <dttm> 2019-01-11 09:24:43, 2019-01-09 09:38:51, 2019-01-09 09:…
    ## $ title        <chr> "banner_click", "banner_show", "banner_show", "banner_sho…
    ## $ target       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

##### 2.2 - Verify any NULL values

``` r
print(colSums(is.na(product)))
```

    ##     order_id      user_id      page_id      product site_version         time 
    ##            0            0            0            0            0            0 
    ##        title       target 
    ##            0            0

##### 2.3 - Data manipulation

``` r
product_df <- product %>% 
  mutate(hour_of_day = as.numeric(format(time, format = "%H"))) %>% 
  mutate_if(is.POSIXt, as.Date) %>% 
  rename(date = "time",
         banner = "product",
         device = "site_version",
         event = "title") %>% 
  mutate(day_of_week = weekdays(date)) %>%
  mutate(week_start_date = as.Date(cut(as.Date(date), "week"), "%Y-%m-%d")-1) %>% 
  mutate(month_start_date = as.Date(as.yearmon(date, format = "%b%Y"), frac = 0)) %>% 
  group_by(device, banner, user_id) %>% 
  mutate(first_session = case_when(date == min(date) ~ 1, TRUE ~ 0)) %>% 
  ungroup() %>% 
  mutate(event = case_when(event == "banner_show" ~ 'impressions',
                           event == "banner_click" ~ 'clicks',
                           event == "order" ~ 'orders',
                           TRUE ~ event))

head(product_df)
```

    ## # A tibble: 6 × 13
    ##   order_id   user_id  page_id  banner device date       event target hour_of_day
    ##   <chr>      <chr>    <chr>    <chr>  <chr>  <date>     <chr>  <dbl>       <dbl>
    ## 1 cfcd20849… c81e728… 6f4922f… sneak… deskt… 2019-01-11 clic…      0           9
    ## 2 c4ca4238a… eccbc87… 4e732ce… sneak… deskt… 2019-01-09 impr…      0           9
    ## 3 c81e728d9… eccbc87… 5c45a86… sport… deskt… 2019-01-09 impr…      0           9
    ## 4 eccbc87e4… eccbc87… fb339ad… compa… deskt… 2019-01-03 impr…      0           8
    ## 5 a87ff679a… eccbc87… fb339ad… compa… deskt… 2019-01-03 clic…      0           8
    ## 6 e4da3b7fb… a87ff67… 182be0c… sport… mobile 2019-01-14 impr…      0           5
    ## # … with 4 more variables: day_of_week <chr>, week_start_date <date>,
    ## #   month_start_date <date>, first_session <dbl>

Within our dataset, we have information about which banners appeared to
the users, whether they clicked on it, as well as information about
their user purchases. <br> - order_id - unique purchase number (NA for
banner clicks and impressions) <br> - user_id - unique identifier of the
client <br> - page_id - unique page number for event bundle (NA for
purchases) <br> - banner - 5 unique products/groups <br> - device - mobile or
desktop <br> - time - time of the action <br> - title - type of event (show, click
or purchase) <br> - target - target class

### \[3\] Perform data analysis

##### 3.1 - Which banner contains the highest number of returning visitors?

``` r
plot1 <- product_df %>% 
  group_by(banner, user_id) %>% 
  mutate(is_returning_user = case_when(n_distinct(date) > 1 ~ 1, 
                                     TRUE ~ 0)) %>% 
  ungroup() %>% 
  group_by(banner) %>% 
  summarise(returning_user_count = n_distinct(user_id[is_returning_user == 1])) %>% 
  ungroup() 

plot1 %>% 
  ggplot(aes(x = reorder(banner, returning_user_count), y = returning_user_count)) +
  geom_bar(stat = 'identity', fill = theme_blue, alpha = 0.9) +
  theme_bw() +
  xlab("banner") +
  ylab("number of returning users")
```

![](https://github.com/philipkfw/TidyTuesday/blob/5ac7ccf2fd8956fca2b06481662b89f07356ce00/Output/TidyTuesday-2022-Wk-17-OnlineSportingGoods_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

##### 3.2 - Which days of the week results in the highest average user traffic (clicks & impressions)?

``` r
plot2 <- product_df %>% 
  filter(event %in% c("clicks","impressions")) %>%
  group_by(day_of_week, event) %>% 
  summarise(avg_user_traffic = round(n() / n_distinct(week_start_date), 0)) %>% 
  ungroup()

ordered_weekDays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
plot2 %>% 
  ggplot(aes(x = factor(day_of_week, level = ordered_weekDays), y = avg_user_traffic, fill = event)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("day of week") +
  ylab("average user traffic") +
  scale_fill_manual(values = c("clicks" = theme_grey, "impressions" = alpha(theme_blue, 0.9)))
```

![](https://github.com/philipkfw/TidyTuesday/blob/5ac7ccf2fd8956fca2b06481662b89f07356ce00/Output/TidyTuesday-2022-Wk-17-OnlineSportingGoods_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

##### 3.3 - What time of day results in the highest average user traffic, given same timezone?

``` r
plot3 <- product_df %>% 
  group_by(hour_of_day, event) %>%
  summarise(avg_user_traffic = round(n() / n_distinct(date), 0)) %>% 
  ungroup()
  
plot3 %>% 
  ggplot(aes(x = as.factor(hour_of_day), y = avg_user_traffic, fill = event)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("hour of day") +
  ylab("average user traffic") +
  facet_wrap(~event, scales = "free") +
  scale_fill_manual(values=c(alpha(theme_blue, 0.9),
                             theme_grey,
                             alpha(theme_purple, 0.9)))
```

![](https://github.com/philipkfw/TidyTuesday/blob/5ac7ccf2fd8956fca2b06481662b89f07356ce00/Output/TidyTuesday-2022-Wk-17-OnlineSportingGoods_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

##### 3.4 - Which banner drove the highest quantity of sales?

``` r
plot4 <- product_df %>% 
  filter(event == 'orders') %>% 
  group_by(banner) %>% 
  summarise(total_orders = n()) %>% 
  ungroup()

plot4 %>% 
  ggplot(aes(x = reorder(banner, total_orders), y = total_orders)) +
  geom_bar(stat = 'identity', fill = alpha(theme_blue, 0.9)) +
  coord_flip() +
  theme_bw() +
  xlab("banner") +
  ylab("total number of orders")
```

![](https://github.com/philipkfw/TidyTuesday/blob/5ac7ccf2fd8956fca2b06481662b89f07356ce00/Output/TidyTuesday-2022-Wk-17-OnlineSportingGoods_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

##### 3.5 - For each banner, what’s the average timespan before users make their first purchase?

``` r
# first session by device, banner, and user
min_date <- product_df %>%
  filter(event != "orders") %>%
  group_by(device, banner, user_id) %>%
  summarise(first_session_date = min(date)) %>%
  ungroup()

# first order date by device, banner, and user
order_date <- product_df %>%
  filter(event == "orders") %>%
  group_by(device, banner, user_id) %>%
  summarise(order_date = min(date)) %>%
  ungroup()

min_date %>%
  left_join(order_date, by = c("device", "banner", "user_id")) %>%
  drop_na(order_date) %>%
  filter(order_date > first_session_date) %>%
  mutate(timespan = as.numeric(difftime(order_date, first_session_date, units = "days"))) %>% 
    ggplot(aes(x = banner, y = timespan, fill = device)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = "banner", y = "number of days to make a purchase") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values=c(alpha(theme_blue, 0.9),
                                alpha(theme_grey, 0.9)))
```

![](https://github.com/philipkfw/TidyTuesday/blob/5ac7ccf2fd8956fca2b06481662b89f07356ce00/Output/TidyTuesday-2022-Wk-17-OnlineSportingGoods_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
