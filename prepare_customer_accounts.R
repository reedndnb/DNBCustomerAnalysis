customers <- data.frame()

for (i in 1:11) {
  filename <- paste(c("customers"),as.character(c(i)), ".csv", collapse="", sep="")
  customers_i <- read.csv(filename, stringsAsFactors=FALSE)
  customers <- rbind(customers, customers_i)
}

# Make relevant variables factors
customers[,c("Primary.City")] <- as.factor(customers[,c("Primary.City")])
customers[,c("Primary.Zip")] <- as.factor(customers[,c("Primary.Zip")])
customers[,c("Primary.Country")] <- as.factor(customers[,c("Primary.Country")])
customers[,c("Line.Of.Business")] <- as.factor(customers[,c("Line.Of.Business")])
customers[,c("Owns.Rents")] <- as.factor(customers[,c("Owns.Rents")])
customers[,c("Is.Importer")] <- as.factor(customers[,c("Is.Importer")])
customers[,c("Is.Exporter")] <- as.factor(customers[,c("Is.Exporter")])
customers[,c("Company.Type")] <- as.factor(customers[,c("Company.Type")])
customers[,c("Is.Subsidiary")] <- as.factor(customers[,c("Is.Subsidiary")])
customers[,c("Location.Type")] <- as.factor(customers[,c("Location.Type")])
customers[,c("Exchange")] <- as.factor(customers[,c("Exchange")])
customers[,c("Is.Manufacturing")] <- as.factor(customers[,c("Is.Manufacturing")])
customers[,c("Is.Women.Owned")] <- as.factor(customers[,c("Is.Women.Owned")])
customers[,c("Is.Minority.Owned")] <- as.factor(customers[,c("Is.Minority.Owned")])
customers[,c("D.B.Prescreen.Score")] <- as.factor(customers[,c("D.B.Prescreen.Score")])
customers[,c("Primary.Industry")] <- as.factor(customers[,c("Primary.Industry")])
customers[,c("Primary.US.SIC.Code")] <- as.factor(customers[,c("Primary.US.SIC.Code")])
customers[,c("Primary.US.NAICS.Code")] <- as.factor(customers[,c("Primary.US.NAICS.Code")])
customers[,c("Postal.Delivery.Point")] <- as.factor(customers[,c("Postal.Delivery.Point")])

# Should be numeric, not character
customers[,c("Revenue.Growth....")] <- as.numeric(customers[,c("Revenue.Growth....")])
customers[,c("Income.Growth....")] <- as.numeric(customers[,c("Income.Growth....")])

# Get customer accounts (with sales)
accounts <- read.csv("customers.csv")
accounts[,c("Sales2013")] <- as.numeric(sub(",","", (sub("\\$","", accounts$Sales2013))))
accounts[,c("Sales2014")] <- as.numeric(sub(",","", (sub("\\$","", accounts$Sales2014))))

library(dplyr)

# Merge with customer list
customer_accounts <- merge(accounts, customers, by.x = "DUNS", by.y = "D.U.N.S.Number")

# Replace missing values for sales with 0's
customer_accounts <- customer_accounts %>% mutate(Sales2013 = ifelse(is.na(Sales2013), 0, Sales2013))
customer_accounts <- customer_accounts %>% mutate(Sales2014 = ifelse(is.na(Sales2014), 0, Sales2014))

# Merge with account data
customer_accounts <- dplyr::select(customer_accounts, 
                            sales2013=Sales2013,
                            sales2014=Sales2014,
                            total_assets=Total.Assets..US.Dollars..million., 
                            market_value=Market.Value..US.Dollars..million.,
                            total_empl=Total.Employees, 
                            empl_growth=Employee.Growth....,
                            primary_sic=Primary.US.SIC.Code, 
                            primary_industry=Primary.Industry,
                            primary_city=Primary.City,
                            revenue_growth=Revenue.Growth....,
                            revenue=Revenue..US.Dollars..million.,
                            net_income=Net.Income..US.Dollars..million.,                 
                            income_growth=Income.Growth....,
                            is_manufacturing=Is.Manufacturing,
                            is_women_owned=Is.Women.Owned,
                            is_minority_owned=Is.Minority.Owned,
                            db_pre_screen=D.B.Prescreen.Score,
                            line_of_business=Line.Of.Business,
                            owns_rents=Owns.Rents,
                            is_importer=Is.Importer,
                            is_exporter=Is.Exporter,
                            company_type=Company.Type,
                            is_subsidiary=Is.Subsidiary,
                            location_type=Location.Type,
                            employees_at_this_location=Employees.At.This.Location) %>%
  mutate(combined_sales=sales2013+sales2014)
customer_accounts$sales2013 <- NULL
customer_accounts$sales2014 <- NULL
