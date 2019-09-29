p8105\_hw2\_ml4418
================
Mengyuan Li
9/28/2019

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(dplyr)
library(ggplot2)
```

# problem 1-part 1

``` r
MrTrash_data = read_excel("trash_data.xlsx", sheet = 1, range = cell_cols("A:N"))
MrTrash_data = janitor::clean_names(MrTrash_data) %>%
  drop_na()

mutate(MrTrash_data, sports_balls = as.integer(round(sports_balls,0)))
```

    ## # A tibble: 285 x 14
    ##    dumpster month  year date                weight_tons volume_cubic_ya~
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31               18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74               13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45               15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06               18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71               13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52               14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76               18
    ## # ... with 275 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>

# problem 1-part 2

``` r
pre2018_data = read_excel("trash_data.xlsx", sheet = 3, skip = 1)
pre2018_data = janitor::clean_names(pre2018_data) %>%
  drop_na()
pre2018_data = mutate(pre2018_data,year = "2018")
  
pre2017_data = read_excel("trash_data.xlsx", sheet = 4, skip = 1)
pre2017_data = janitor::clean_names(pre2017_data) %>%
  drop_na()
pre2017_data = mutate(pre2017_data, year = "2017")

pre_data = bind_rows(pre2018_data, pre2017_data) %>% 
  mutate(month = month.name[month])
```

Conclusion: In Mr.Trash Wheel, the total number of observations is 285.
In precipitation dataset, the total number of observations in 2018 is 7,
and the total number of observations in 2017 is 12. Specifically, the
total precipitation in 2018 is 23.5. Also, the median number of sports
balls in a dumpster in 2017 is 8

# problem 2-clean data in pols-month

``` r
pols_month_data = read.csv("pols-month.csv") %>%
  separate(mon, into = c("month", "day", "year"), "/") %>%
  mutate(month = as.integer(month)) %>%
  mutate(month = month.name[month]) %>%
  mutate(president = prez_gop + prez_dem) %>%
  select(-prez_gop, -prez_dem , -day)
```

# problem 2-clean date in snp

``` r
snp_data = read.csv("snp.csv") %>%
  separate(date, into = c("month","day","year"), "/") %>%
  mutate(month = as.integer(month)) %>%
  mutate(month = month.name[month]) %>%
  select(year, month, -day, close)
```

# problem 2-tidy unemployment data

``` r
un_data = read.csv("unemployment.csv")
tidy_un_data = 
  pivot_longer(un_data,
               Jan:Dec,
               names_to = "month",
               values_to = "unempolyment") 
tidy_un_data = janitor::clean_names(tidy_un_data) %>%
  mutate(year = as.character(year))
  


join_data = 
  left_join(pols_month_data, snp_data, by = c("month","year"))
final_data = 
  left_join(join_data, tidy_un_data, by = c("month","year"))
```

Conclusion: The dimension of snp dataset is 787\* 3. The dimension of
pols-month dataset is 822 \* 9. The dimension of unemployment dataset is
816 \* 3. The dimension of the final dataset is 822 \* 11. The range of
years in snp dataset is from 1950 to 2015. The range of years in
pols-month dataset is from 1947 to 2015. The range of years in
unempolyment dataset is from 1948 to 2015. THe rangeof years in the
final dataset is from 1947 to 2015.

# problem 3-part 1

``` r
popular_baby_names = read.csv("Popular_Baby_Names.csv")
popular_baby_names = janitor::clean_names(popular_baby_names)
popular_baby_names = unique(popular_baby_names)
```

# problem 3-part 2

``` r
olivia_name_data = filter(popular_baby_names, child_s_first_name == "Olivia") %>%
  select(ethnicity, year_of_birth, rank)
olivia_name_data = pivot_wider(olivia_name_data, 
                               ethnicity:rank, 
                               names_from = "year_of_birth",
                               values_from = "rank")
```

# problem 3-part 3

``` r
popular_male_names = filter(popular_baby_names, gender == "MALE") %>%
  select(year_of_birth, child_s_first_name, count)

popular_male_names_2016 = filter(popular_male_names, year_of_birth == "2016") 
popular_male_names_2016 = filter(popular_male_names_2016, count == max(count))

popular_male_names_2015 = filter(popular_male_names, year_of_birth == "2015")
popular_male_names_2015 = filter(popular_male_names_2015, count == max(count))

popular_male_names_2014 = filter(popular_male_names, year_of_birth == "2014")
popular_male_names_2014 = filter(popular_male_names_2014, count == max(count))

popular_male_names_2013 = filter(popular_male_names, year_of_birth == "2013")
popular_male_names_2013 = filter(popular_male_names_2013, count == max(count))

popular_male_names_2012 = filter(popular_male_names, year_of_birth == "2012")
popular_male_names_2012 = filter(popular_male_names_2012, count == max(count))

popular_male_names_2011 = filter(popular_male_names, year_of_birth == "2011")
popular_male_names_2011 = filter(popular_male_names_2011, count == max(count))

most_popular_male_name = bind_rows(popular_male_names_2016,
                                   popular_male_names_2015,
                                   popular_male_names_2014,
                                   popular_male_names_2013,
                                   popular_male_names_2012,
                                   popular_male_names_2011)
```

# problem 3- part 4

``` r
white_males_2016 = filter(popular_baby_names, 
                          gender == "MALE",
                          ethnicity == "WHITE NON HISPANIC",
                          year_of_birth == "2016")
white_males_2016_plot = ggplot(white_males_2016,
                               aes(x = rank, y= count)) + geom_point(aes(color = "lightpink"))
```
