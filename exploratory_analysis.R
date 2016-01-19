# This script contains code which is used for plots and analysis
# of the data.

# What are the relationships bewteen market value, net income, revenue and sales?
pairs(~combined_sales + revenue + net_income + market_value, data=customer_accounts)

plot(customer_accounts$market_value, 
     customer_accounts$combined_sales, 
     xlab="Market value (USD)", 
     ylab="Combined Sales (USD)",
     main="Market Value vs. Combined Sales (2013, 2014) in USD")
modFit <- lm(combined_sales~market_value, data=customer_accounts)
abline(modFit)


# Do we sell more to fast-growing companies than slow-growing companies?

# Which industries (SIC) have the highest sales? Are particular types of businesses (Is.Manufacturing, Is.Women.Owned, Is.Minority.Owned)
# predictive of sales?

by_primary_industry <- group_by(customer_accounts, primary_industry)
sales_by_primary_industry <- summarize(by_primary_industry, sales=sum(combined_sales))
sales_by_primary_industry <- arrange(sales_by_primary_industry, desc(sales))
sales_by_primary_industry[1:10,c("primary_industry", "sales")]


# Which cities have the highest sales?
