Birth\_data
================
Xue Yang
12/3/2018"

#### Load and clean the data from community\_district source

``` r
birth_data = 
  tibble(file_name = list.files(path = "./birth_data/community_district")) %>% 
  
  # iterate over file names and read in data for each subject
  mutate(output = purrr::map(str_c("./birth_data/community_district/", file_name), haven::read_sas)) %>%
  
  # unnest the dataframe
  unnest() %>% 
  separate(file_name, c("name", "year", "del"), sep = c(6, 8)) %>% 
  select(-name, -del) %>%
  mutate(year = as.factor(year))

# import cd code data
cd_code_data = 
  read_csv("./data/New_York_City_Population_By_Community_Districts 16.09.03.csv") %>%
  janitor::clean_names() %>%
  select(borough, cd_number, cd_name) %>% 
  rename(cd = cd_number) %>% 
  mutate(cd = ifelse(borough == "Bronx", cd + 200, cd)) %>% 
  mutate(cd = ifelse(borough == "Brooklyn", cd + 300, cd)) %>% 
  mutate(cd = ifelse(borough == "Manhattan", cd + 100, cd)) %>% 
  mutate(cd = ifelse(borough == "Queens", cd + 400, cd)) %>% 
  mutate(cd = ifelse(borough == "Staten Island", cd + 500, cd)) 
```

    ## Parsed with column specification:
    ## cols(
    ##   Borough = col_character(),
    ##   `CD Number` = col_integer(),
    ##   `CD Name` = col_character(),
    ##   `1970 Population` = col_integer(),
    ##   `1980 Population` = col_integer(),
    ##   `1990 Population` = col_integer(),
    ##   `2000 Population` = col_integer(),
    ##   `2010 Population` = col_integer()
    ## )

``` r
# join two data
birth_data_un = 
  birth_data %>% 
  unnest() %>% 
  left_join(cd_code_data, by = "cd") 
```

#### Deal with missing value

``` r
# check for the number of missing value
missing_value = 
  birth_data_un %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(term, num_na, everything()) %>% 
  mutate(percent = num_na/885) %>% 
  filter(percent > 0.2)
```

For the missing value, we count for the number of missing value for each column of the variables and then count for the proportion of them. Using the cutoff = 20%. If the proprtion of missing value is larger than 20%, we delete this column of variable(usually this means to delete a category of one specific variable). And if the proportion is less than 20%, we keep this catagory of the specific variable and valua the NA as the same value as it was in the last yeat of the same borough.

#### Tidy the data for each variables separately

**Individual Variables**

age, nativity, parity, maternal marital status, infant sex, infant birthweight

``` r
# checking missing data for the data maternal age
birth_data_un %>% 
  select(year, cd, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl) %>% 
  arrange(desc(delta))
```

    ## # A tibble: 885 x 5
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>
    ##  1 03      102      842   833     9
    ##  2 12      105      579   570     9
    ##  3 00      103     2320  2312     8
    ##  4 00      401     2533  2525     8
    ##  5 02      316     1446  1438     8
    ##  6 02      411      675   667     8
    ##  7 02      501     2447  2439     8
    ##  8 03      209     2694  2686     8
    ##  9 05      112     2892  2884     8
    ## 10 05      313     1171  1163     8
    ## # ... with 875 more rows

Since the delta (the difference between total number and the non-missing number) is very small (less equal than 9). So we can delete some category with high proportion of missing data. In this case, there won't be much change in the data.

So for the age variable, we delete the category "age1tot", "age9tot", (with missing proportion &gt;20%), for the rest category, we fill the NA with the data from last year in the same community distric.

``` r
# data for maternal age

maternal_age_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>%
  arrange(cd, maternal_age, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, maternal_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = maternal_age, value = number) %>% 
  knitr::kable(digits = 3)

maternal_age_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>% 
  group_by(year, borough, maternal_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = maternal_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal nativity

birth_data_un %>% 
  select(year, cd, birthtot, nat1tot:nat2tot) %>%
  gather(born_demo, num, nat1tot:nat2tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 03      212     1919  1875    44  0.0229
    ##  2 02      104      818   800    18  0.0220
    ##  3 01      212     1964  1921    43  0.0219
    ##  4 00      103     2320  2275    45  0.0194
    ##  5 03      110     1669  1638    31  0.0186
    ##  6 05      203     1293  1271    22  0.0170
    ##  7 04      105      481   473     8  0.0166
    ##  8 02      103     2167  2131    36  0.0166
    ##  9 02      202      912   897    15  0.0164
    ## 10 06      203     1353  1331    22  0.0163
    ## # ... with 875 more rows

``` r
ma_nat_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot:nat2tot) %>%
  gather(ma_nat, num, nat1tot:nat2tot) %>%
  arrange(cd, ma_nat, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, ma_nat) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = ma_nat, value = number) %>% 
  knitr::kable(digits = 3)


born_demo_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot:nat2tot) %>%
  gather(born_demo, num, nat1tot:nat2tot) %>% 
  group_by(year, borough, born_demo) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = born_demo, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for parity

birth_data_un %>% 
  select(year, cd, birthtot, par1tot:par2tot) %>%
  gather(parity, num, par1tot:par2tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 09      317     2264  2242    22 0.00972
    ##  2 13      303     2323  2306    17 0.00732
    ##  3 09      308     1461  1451    10 0.00684
    ##  4 09      309     1682  1672    10 0.00595
    ##  5 10      316     1445  1437     8 0.00554
    ##  6 09      305     2895  2879    16 0.00553
    ##  7 08      103     2215  2203    12 0.00542
    ##  8 09      303     2512  2499    13 0.00518
    ##  9 09      318     2539  2527    12 0.00473
    ## 10 10      304     1934  1925     9 0.00465
    ## # ... with 875 more rows

``` r
parity_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, par1tot:par2tot) %>%
  gather(parity, num, par1tot:par2tot) %>%
  arrange(cd, parity, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, parity) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = parity, value = number) %>% 
  knitr::kable(digits = 3)
  

parity_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, par1tot:par2tot) %>%
  gather(parity, num, par1tot:par2tot) %>% 
  group_by(year, borough, parity) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = parity, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal marital status 

birth_data_un %>% 
  select(year, cd, birthtot, mar1tot:mar2tot) %>%
  gather(marry_status, num, mar1tot:mar2tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta  percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>    <dbl>
    ##  1 02      205     2596  2595     1 0.000385
    ##  2 00      101      436   436     0 0       
    ##  3 00      102      745   745     0 0       
    ##  4 00      103     2320  2320     0 0       
    ##  5 00      104      777   777     0 0       
    ##  6 00      105      436   436     0 0       
    ##  7 00      106     1259  1259     0 0       
    ##  8 00      107     2568  2568     0 0       
    ##  9 00      108     2734  2734     0 0       
    ## 10 00      109     1584  1584     0 0       
    ## # ... with 875 more rows

``` r
marry_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, mar1tot:mar2tot) %>%
  gather(marry_status, num, mar1tot:mar2tot) %>%
  arrange(cd, marry_status, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, marry_status) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = marry_status, value = number) %>% 
  knitr::kable(digits = 3)

marry_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, mar1tot:mar2tot) %>%
  gather(marry_status, num, mar1tot:mar2tot) %>% 
  group_by(year, borough, marry_status) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = marry_status, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex
birth_data_un %>% 
  select(year, cd, birthtot, sex1tot:sex2tot) %>%
  gather(infant_sex, num, sex1tot:sex2tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 00      101      436   436     0       0
    ##  2 00      102      745   745     0       0
    ##  3 00      103     2320  2320     0       0
    ##  4 00      104      777   777     0       0
    ##  5 00      105      436   436     0       0
    ##  6 00      106     1259  1259     0       0
    ##  7 00      107     2568  2568     0       0
    ##  8 00      108     2734  2734     0       0
    ##  9 00      109     1584  1584     0       0
    ## 10 00      110     1785  1785     0       0
    ## # ... with 875 more rows

``` r
infant_sex_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot:sex2tot) %>%
  gather(infant_sex, num, sex1tot:sex2tot) %>%
  arrange(cd, infant_sex, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, infant_sex) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = infant_sex, value = number) %>% 
  knitr::kable(digits = 3)

infant_sex_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot:sex2tot) %>%
  gather(infant_sex, num, sex1tot:sex2tot) %>% 
  group_by(year, borough, infant_sex) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = infant_sex, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant birthweight
birth_data_un %>% 
  select(year, cd, birthtot, bwt1tot:bwt9tot) %>%
  gather(bwt, num, bwt1tot:bwt9tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 00      105      436   426    10  0.0229
    ##  2 03      105      496   485    11  0.0222
    ##  3 05      105      482   473     9  0.0187
    ##  4 12      105      579   570     9  0.0155
    ##  5 06      105      474   467     7  0.0148
    ##  6 00      101      436   430     6  0.0138
    ##  7 14      105      545   539     6  0.0110
    ##  8 03      104      834   825     9  0.0108
    ##  9 03      101      565   559     6  0.0106
    ## 10 04      105      481   476     5  0.0104
    ## # ... with 875 more rows

``` r
birth_weight_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, bwt1tot:bwt9tot) %>%
  gather(birth_weight, num, bwt1tot:bwt9tot) %>%
  arrange(cd, birth_weight, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, birth_weight) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = birth_weight, value = number) %>% 
  knitr::kable(digits = 3)

birth_weight_data =  
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, bwt1tot:bwt9tot) %>%
  gather(birth_weight, num, bwt1tot:bwt9tot) %>% 
  group_by(year, borough, birth_weight) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = birth_weight, value = number) %>% 
  knitr::kable(digits = 3)
```

**Crossed Variables** infant birthweight and maternal age, infant sex and maternal nativity, maternal nativity and maternal age, gestational and maternal age, infant sex and maternal age

``` r
# data for infant birthweight and maternal age
birth_data_un %>% 
  select(year, cd, birthtot, contains("ctot_a")) %>%
  select(year, cd, birthtot, starts_with("bwt")) %>%
  gather(bwt, num, bwt1ctot_a1:bwt2ctot_a4) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 00      101      436   425    11 0.0252 
    ##  2 01      105      405   398     7 0.0173 
    ##  3 02      105      423   416     7 0.0165 
    ##  4 02      101      389   384     5 0.0129 
    ##  5 04      105      481   476     5 0.0104 
    ##  6 06      101      760   753     7 0.00921
    ##  7 14      102      764   757     7 0.00916
    ##  8 13      210      952   944     8 0.00840
    ##  9 03      105      496   492     4 0.00806
    ## 10 02      102      769   763     6 0.00780
    ## # ... with 875 more rows

``` r
bwt_age_nomiss_data =
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_a")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("bwt")) %>% 
  gather(bwt_age, num, bwt1ctot_a1:bwt2ctot_a4) %>%
  arrange(cd, bwt_age, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, bwt_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = bwt_age, value = number) %>% 
  knitr::kable(digits = 3)

bwt_age_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_a")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("bwt")) %>% 
  gather(bwt_age, num, bwt1ctot_a1:bwt2ctot_a4) %>% 
  group_by(year, borough, bwt_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = bwt_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex and maternal nativity
birth_data_un %>% 
  select(year, cd, birthtot, contains("tot_n")) %>%
  select(year, cd, birthtot, starts_with("sex")) %>%
  gather(sex, num, sex1tot_n1:sex2tot_n2) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 03      212     1919  1875    44  0.0229
    ##  2 02      104      818   800    18  0.0220
    ##  3 01      212     1964  1921    43  0.0219
    ##  4 00      103     2320  2275    45  0.0194
    ##  5 03      110     1669  1638    31  0.0186
    ##  6 05      203     1293  1271    22  0.0170
    ##  7 04      105      481   473     8  0.0166
    ##  8 02      103     2167  2131    36  0.0166
    ##  9 02      202      912   897    15  0.0164
    ## 10 06      203     1353  1331    22  0.0163
    ## # ... with 875 more rows

``` r
sex_nat_nomiss_data =
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("tot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("sex")) %>% 
  gather(sex_nat, num, sex1tot_n1:sex2tot_n2) %>%
  arrange(cd, sex_nat, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, sex_nat) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_nat, value = number) %>% 
  knitr::kable(digits = 3)


sex_nat_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("tot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("sex")) %>% 
  gather(sex_nat, num, sex1tot_n1:sex2tot_n2) %>% 
  group_by(year, borough, sex_nat) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_nat, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal nativity and maternal age
birth_data_un %>% 
  select(year, cd, birthtot, nat1tot_a1, nat2tot_a1, nat1tot_a2, nat2tot_a2, nat1tot_a3, nat2tot_a3, nat1tot_a4, nat2tot_a4) %>%
  gather(nativity_age, num, nat1tot_a1:nat2tot_a4) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 03      102      842   821    21  0.0249
    ##  2 03      212     1919  1875    44  0.0229
    ##  3 02      104      818   800    18  0.0220
    ##  4 01      212     1964  1921    43  0.0219
    ##  5 00      103     2320  2275    45  0.0194
    ##  6 04      105      481   472     9  0.0187
    ##  7 03      110     1669  1638    31  0.0186
    ##  8 02      101      389   382     7  0.0180
    ##  9 04      101      616   605    11  0.0179
    ## 10 05      203     1293  1271    22  0.0170
    ## # ... with 875 more rows

``` r
birth_nat_age_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot_a1, nat2tot_a1, nat1tot_a2, nat2tot_a2, nat1tot_a3, nat2tot_a3, nat1tot_a4, nat2tot_a4) %>%
  gather(nativity_age, num, nat1tot_a1:nat2tot_a4) %>%
  arrange(cd, nativity_age, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, nativity_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = nativity_age, value = number) %>% 
  knitr::kable(digits = 3)

birth_nat_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot_a1, nat2tot_a1, nat1tot_a2, nat2tot_a2, nat1tot_a3, nat2tot_a3, nat1tot_a4, nat2tot_a4) %>% 
  gather(key = nativity_age, value = num, nat1tot_a1:nat2tot_a4) %>% 
  group_by(year, borough, nativity_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = nativity_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex and maternal age
birth_data_un %>% 
  select(year, cd, birthtot, sex1tot_a1, sex2tot_a1, sex1tot_a2, sex2tot_a2, sex1tot_a3, sex2tot_a3, sex1tot_a4, sex2tot_a4) %>%
  gather(sex_age, num, sex1tot_a1:sex2tot_a4) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl, percent = delta/birthtot) %>% 
  arrange(desc(percent))
```

    ## # A tibble: 885 x 6
    ## # Groups:   year, cd [885]
    ##    year     cd birthtot   ttl delta percent
    ##    <fct> <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ##  1 12      105      579   572     7 0.0121 
    ##  2 13      105      599   592     7 0.0117 
    ##  3 03      411      715   708     7 0.00979
    ##  4 14      105      545   540     5 0.00917
    ##  5 06      105      474   470     4 0.00844
    ##  6 02      101      389   386     3 0.00771
    ##  7 07      105      523   519     4 0.00765
    ##  8 10      411      691   686     5 0.00724
    ##  9 09      105      556   552     4 0.00719
    ## 10 10      105      569   565     4 0.00703
    ## # ... with 875 more rows

``` r
birth_sex_age_nomiss_data =
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot_a1, sex2tot_a1, sex1tot_a2, sex2tot_a2, sex1tot_a3, sex2tot_a3, sex1tot_a4, sex2tot_a4) %>%
  gather(sex_age, num, sex1tot_a1:sex2tot_a4) %>%
  arrange(cd, sex_age, year) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num), num)) %>% 
  mutate(num = ifelse(is.na(num) & year != "00", lag(num, 2), num)) %>% 
  mutate(num = ifelse(is.na(num), 0, num)) %>% 
  group_by(year, borough, sex_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_age, value = number) %>% 
  knitr::kable(digits = 3)

birth_sex_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot_a1, sex2tot_a1, sex1tot_a2, sex2tot_a2, sex1tot_a3, sex2tot_a3, sex1tot_a4, sex2tot_a4) %>% 
  gather(key = sex_age, value = num, sex1tot_a1:sex2tot_a4) %>% 
  group_by(year, borough, sex_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_age, value = number) %>% 
  knitr::kable(digits = 3)
```
