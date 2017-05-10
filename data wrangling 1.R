#load packages and import dataset
library(readr)
refine_original <- read_csv("~/Documents/refine_original.csv")

#clean up brand names
refine_step1 <- refine_original %>%
  mutate(company = tolower(company)) %>% 
  mutate(company = case_when(
    .$company %in% c('phillips', 'phillps', 'phllips', 'fillips', 'phlips') ~ 'philips',
    .$company %in% c('akz0', 'ak zo') ~ 'akzo',
    .$company %in% c('unilvr', 'unlever', 'unilver') ~ 'unilever',
    TRUE ~ .$company)
  )
      
#separate product code and number
refine_step2 <- separate(refine_step1, 'Product code / number', into = c("product_code", "product_number"))

#add product categories
refine_step3 <- refine_step2 %>% 
  mutate(product_category = case_when(
    .$product_code == "p" ~ "Smartphone",
    .$product_code == "v" ~ "TV",
    .$product_code == "x" ~ "Laptop",
    .$product_code == "q" ~ "Tablet"))

#add full address for geocoding
refine_step4 <- unite(refine_step3, address, city, country, col = "full_address", sep = ",")

#create dummy variables for company and product category
refine_final <- refine_step4 %>%
  mutate(company_binary = 1, product_binary = 1) %>%
  mutate(company = paste("company",sep = "_", company), product_category = paste("product", sep = "_", product_category)) %>% 
  spread(company, company_binary, fill = 0) %>%
  spread(product_category, product_binary, fill = 0)

#save the new dataframe
write_csv(refine_final, "refine_clean.csv")
       