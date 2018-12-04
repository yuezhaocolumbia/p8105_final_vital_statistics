Birth\_data
================
Xue Yang
12/3/2018"

#### Load the data from community\_district source

``` r
birth_data = 
  tibble(file_name = list.files(path = "./birth_data/community_district")) %>% 
  
  # iterate over file names and read in data for each subject
  mutate(output = purrr::map(str_c("./birth_data/community_district/", file_name), haven::read_sas)) %>%
  
  # unnest the dataframe
  unnest() 
```
