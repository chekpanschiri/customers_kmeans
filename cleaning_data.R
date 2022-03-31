library(readxl)
library(tidyverse)

# Open Files

bikes_tbl <- read_excel(path = "Data/bikes.xlsx")

bikeshops_tbl <- read_excel(path="Data/bikeshops.xlsx")

orderlines_tbl <- read_excel(path="Data/orderlines.xlsx")


bike_ordelines_joined_tbl <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

glimpse(bike_ordelines_joined_tbl)



bike_orderlines_wrangled_tbl <- bike_ordelines_joined_tbl %>% 
  separate(description, into = c("category.1", "category.2", "frame.material"),
           sep = " - ",
           remove=TRUE) %>% 
  separate(location, into = c("city", "state"),
           sep= ", ",
           remove = FALSE) %>% 
  
  #adding total price with mutate()
  
  mutate(total.price = price * quantity) %>% 
  
  #reorganize
  select(-...1, -location) %>% 
  select(-ends_with(".id")) %>% 
  
  bind_cols(bike_ordelines_joined_tbl %>% select(order.id)) %>% 
  
  # reorder columns
  
  select(contains("date"), contains("id"), contains("order"),
         quantity, price, total.price,
         everything()) %>% 
  
  rename(order_date = order.date) %>% 
  
  set_names(names(.) %>% str_replace_all("\\.","_"))


bike_orderlines_wrangled_tbl %>% glimpse()


bike_orderlines_wrangled_tbl %>% 
  write_rds("Data/bike_orderlines.rds")
