birth\_stat\_kk
================
Kangkang Zhang

Three different source of the data: community district, zip code and census tract

``` r
# community district
file_name = tibble(
  id = list.files("./birth_data/community_district")
)

birth_data_cd = file_name %>% 
  mutate(infor = map(str_c("./birth_data/community_district/", id), read_sas))
 
birth_data_cd_un = birth_data_cd %>% 
  separate(id, c("name", "year", "del"), sep = c(6, 8)) %>% 
  select(-name, -del) %>% 
  unnest() 
```
