# Chad Bronson 12/20/2019
# import libraries
library(dplyr)
library(tibble)
library(readr)


# set working directory
setwd("~/Not Work/Foundations of Data Management/FinalProject/CSV")

# creating variable names for csv's
prodsubcat <- read_csv("prodsubcat.csv", col_names = TRUE, col_types = "ccc")
prodseg <- read_csv("prodseg.csv", col_names = TRUE)
prodmaster <- read_csv("prodmaster.csv", col_names = TRUE, col_types = "cccccc")
prodcat <- read_csv("prodcat.csv", col_names =  TRUE)
postrans <- read_csv("postrans.csv", col_names = TRUE, col_types = "cccidi")
pharmacymaster <- read_csv("pharmacymaster.csv", col_names = TRUE, col_types = "cccc")
majorprodcat <- read_csv("majorprodcat.csv", col_names =   TRUE)

# formatting date for postrans table:
postrans$SLS_DTE_NBR <- strptime(postrans$SLS_DTE_NBR, format = "%Y%m%d")
postrans$SLS_DTE_NBR <- strftime(postrans$SLS_DTE_NBR, format = "%Y-%m-%d")


# -----------------------------------------BUSINESS QUESTIONS-------------------------------------------------------------------------
# ////////////////////////////////////////////////////// QUESTION 1 /////////////////////////////////////////////////////////
# 1. What product sold the most (highest quantity)? Is that the same as the product that brought in the most revenue?
# ANSWER: product number 90400000000 is the highest selling (quantity).
# Product with highest revenue is money order, money gram is second, 90400000000 is third.
postrans %>% group_by(PROD_NBR) %>% summarize(sale_qty = Reduce('+', SLS_QTY)) %>% top_n(1)

# top_n(3) is used since some may or may not consider money orders and money grams as true products.
# Third in the list is the first 'traditional' product
postrans %>% group_by(PROD_NBR) %>% summarize(sale_price = Reduce('+', EXT_SLS_AMT)) %>%
      top_n(3) %>% arrange(desc(sale_price))


# ////////////////////////////////////////////////////// QUESTION 2 /////////////////////////////////////////////////////////
# 2. What was the most returned product?
# ANSWER: product number 98650000000000944904150 had 146 returns. The product is a wheeled walker.
func_1 <- function(){
  a = postrans %>% inner_join(prodmaster, by = c("PROD_NBR" = "PROD_NBR"))%>%
      filter(SLS_QTY < 0) %>%
      group_by(PROD_NBR) %>%
      summarize(returns = Reduce("+", SLS_QTY)) %>%
      top_n(-1)
  
  return(a)
}


prodmaster %>% inner_join(func_1(), by = c("PROD_NBR" = "PROD_NBR")) %>%
      select(PROD_NBR, PROD_DESC, returns)

        

# ////////////////////////////////////////////////////// QUESTION 3 /////////////////////////////////////////////////////////
# 3. Were there any products that resulted in a net negative value both in quantity and sales?
# ANSWER: 19 products resulted in loss, both in terms of returns and return value.
postrans %>% group_by(PROD_NBR) %>% filter(Reduce('+', SLS_QTY) < 0) %>%
       summarize(returns = Reduce('+', SLS_QTY), return_value = Reduce('+', EXT_SLS_AMT))

# ////////////////////////////////////////////////////// QUESTION 4 /////////////////////////////////////////////////////////
# 4. What product had the lowest net revenue? Is that the same product that had the most net returns?
# ANSWER: Product number 68289138810 lost the most money ($77, this product is a universal blood pressure cuff).
# ANSWER: Product number 73588290395 had the most net negative returns (3 more returns than sales, this product is a birhtday card)
# So, the product with the lowest revenue is not the product with the most net returns.
postrans %>% group_by(PROD_NBR) %>% summarize(sale_qty = Reduce('+', EXT_SLS_AMT)) %>% top_n(-1)


postrans %>% group_by(PROD_NBR) %>% summarize(sale_qty = Reduce('+', SLS_QTY)) %>% top_n(-1)




# ////////////////////////////////////////////////////// QUESTION 5 /////////////////////////////////////////////////////////
#	5. What is the average purchase quantity (number of items) per basket ID? What's the average price? Min? Max?
# max purchase qty: 320, max sale amount: $75000, largest return: 36, largest return amount: $2,065.95
# avg purchase qty: 1.82, avg sale amount : $14
postrans %>% summarize(max_purchase_qty = max(SLS_QTY),
                       largest_return_qty = min(SLS_QTY),
                       avg_purchase_qty = sum(SLS_QTY)/n_distinct(BSKT_ID),
                       max_sale_amt = max(EXT_SLS_AMT),
                       largest_return_amt = min(EXT_SLS_AMT),
                       avg_sale_amt = sum(EXT_SLS_AMT)/n_distinct(BSKT_ID))




# ////////////////////////////////////////////////////// QUESTION 6 /////////////////////////////////////////////////////////
# 6. How many products didn't sell at all (if any)?
# ANSWER: 145,624 products have no sales record during this 6 month time frame. This means only 43,428 different products sold.
tally(anti_join(prodmaster, postrans, by = c("PROD_NBR" = "PROD_NBR")))






# ////////////////////////////////////////////////////// QUESTION 7 /////////////////////////////////////////////////////////
# 7. How many products sold per state? How much revenue per state?
# ANSWER is in the View
View(prodmaster %>% inner_join(postrans, by = c("PROD_NBR" = "PROD_NBR")) %>%
        inner_join(pharmacymaster, by = c("PHRMCY_NBR" = "PHRMCY_NBR")) %>%
        group_by(ST_CD) %>%
        summarize(state_sls_qty = Reduce("+", SLS_QTY),
                  state_rev = Reduce("+", EXT_SLS_AMT), num_of_stores = n_distinct(PHRMCY_NBR)) %>%
        arrange(desc(state_rev)))



# ////////////////////////////////////////////////////// QUESTION 8 /////////////////////////////////////////////////////////
# 8. What category of products sells the most? The least?
# ANSWER: Greeting cards and candy were the top two sellers in terms of quantity.
# Money orders generated the most revenue.
# Category of product with the most revenue outside of money orders was greeting cards
# two queries were written to get the top two values for EACH. 1 query would result in the top two where SLS_QTY & EXT_SLS_AMT are combined.
postrans %>% inner_join(prodmaster, by = c("PROD_NBR" = "PROD_NBR")) %>%
  inner_join(prodsubcat, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  group_by(SUB_CAT_CD) %>%
  summarize(revenue = Reduce("+", EXT_SLS_AMT)) %>%
  top_n(2)

postrans %>% inner_join(prodmaster, by = c("PROD_NBR" = "PROD_NBR")) %>%
  inner_join(prodsubcat, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
  group_by(SUB_CAT_CD) %>%
  summarize(sls_quantity = Reduce("+",SLS_QTY)) %>%
  top_n(2)



# ////////////////////////////////////////////////////// QUESTION 9 /////////////////////////////////////////////////////////
# 9. What was the highest-selling product per state?
# ANSWER is in the View
subq <- function(){
  x <- pharmacymaster %>% inner_join(postrans, by = c("PHRMCY_NBR" = "PHRMCY_NBR")) %>%
        inner_join(prodmaster, by = c("PROD_NBR" = "PROD_NBR")) %>%
        inner_join(prodsubcat, by = c("SUB_CAT_CD" = "SUB_CAT_CD")) %>%
        group_by(PROD_NBR, ST_CD, PROD_DESC, SUB_CAT_DESC) %>%
        summarize(qty_sold = sum(SLS_QTY))
  
  return(x)
}

View(subq() %>% group_by(ST_CD) %>% top_n(1) %>% arrange(desc(qty_sold)))
        


# ////////////////////////////////////////////////////// QUESTION 10 /////////////////////////////////////////////////////////
# 10. What was the busiest month of these 6 months data were recorded for?
# ANSWERS: June had the highest revenue, April had the lowest.
# March had the highest sales quantity, April had the fewest.
# March had the most transactions, January had the fewest.
postrans %>% 
    mutate(month_name = months(as.Date(SLS_DTE_NBR))) %>%
    group_by(month_name) %>%
    summarize(rev_per_month = Reduce("+", EXT_SLS_AMT), sls_qty_month = Reduce("+", SLS_QTY),
              pos_transactions = n_distinct(BSKT_ID)) %>%
    arrange(match(month_name, month.name))










