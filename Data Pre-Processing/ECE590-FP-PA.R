#load libraries and packages
library(dplyr)
library(readxl)

online_retailer_data = read_excel("C:\\Users\\LBlac\\Downloads\\Online Retail.xlsx")
online_retailer_data2 = read_excel("C:\\Users\\LBlac\\Downloads\\online_retail_II.xlsx")

#copy original dataset
retail2 = online_retailer_data;
retail = online_retailer_data2

#change column names for each dataset to match each other
colnames(retail2)[1] = 'Invoice'
colnames(retail2)[6] = 'Price'
colnames(retail)[7] = 'CustomerID'

#see max dates of retail2
retail2 %>%
  select(InvoiceDate) %>%
  count(InvoiceDate) %>%
  arrange(InvoiceDate)

#see min dates of retail
retail %>%
  select(InvoiceDate) %>%
  count(InvoiceDate) %>%
  arrange(desc(InvoiceDate))

#there will be duplicate transactions in the combined dataset due to the 
# overlapping data ranges for the datasets (10 days worth).
# Best approach is to capture duplicate invoices
retail$presentin2 = as.integer(retail$Invoice %in% retail2$Invoice)

overlap = retail[retail$presentin2 == 1, "Invoice"]
retail3 = retail2[!(retail2$Invoice %in% unlist(overlap)),]
retail = retail[,1:length(retail)-1]

colnames(retail3)
colnames(retail)

#combine datasets together
df = rbind(retail,retail3)

#start pre-analysis, there are missing values in the description and customer id
# fields. We won't be able to do anything for the missing data in the customer
# id field, however there are a few options for the description field
colSums(is.na(df))

#There are many transactions that have a negative quantity for items.
# Many times, these are due to cancellations on orders, however they may also
# be due data errors. Luckily, for the analysis we will be conducting, we only
# care about orders that have actually been processed, so we can treat them
# the same.
df[df$Quantity < 0, ] %>% arrange(-desc(Quantity))
df[df$Invoice == 556687,]

#There are also items that have a negative price, and some that have a price
# equal to 0.
sum(df$Price < 0)
sum(df$Price == 0)

#Now that we have a good idea of areas of data that need to be addressed, we 
# can do some preprocessing. First, there are varying prices for items.
# This can usually be attributed to sales and varying pricing for items.
# However, we can also use this to correct items that have a price of $0.
allstockPrice = df[df$Price != 0,] %>%
  select(StockCode, Price) %>%
  group_by(StockCode, Price) %>%
  summarise(timesCounted = n())

#Find most common price for each item
mostCommonStockPriceQuant = allstockPrice %>%
  group_by(StockCode) %>%
  summarise(timesCounted = max(timesCounted)) 

stockPrice = merge(x = mostCommonStockPriceQuant, y = allstockPrice)

#When there is an item that has more than one mode (i.e. 3 occurences of $15
# and 3 occurenes of $20), the stockPrice data captures all of them. This will
# lead to errors in mapping the prices to the actual dataset if not corrected.
# The approach taken is to use the max price of the modes ($20 for the example
# given), and delete the other rows.
for (i in unique(stockPrice$StockCode)){
  stockPrice[stockPrice$StockCode == i,"Price"] =
    max(stockPrice[stockPrice$StockCode == i,"Price"])
}
stockPrice = distinct(stockPrice)
zerostocks = df[!(df$StockCode %in% stockPrice$StockCode),"StockCode"]
zerostocks = data.frame("StockCode" = zerostocks,
                        "timesCounted" = 0,
                        "Price" = 0)
zerostocks = distinct(zerostocks)
stockPrice = rbind(stockPrice,zerostocks)

#Replace $0 prices with most common actual prices. Some values will still be $0,
# which is fine as there are no alternatives available.
for (i in 1:dim(df)[1]){
  if (df[i,"Price"] == 0) {
    df[i,"Price"] = stockPrice[stockPrice$StockCode 
                               == as.character(df[i,"StockCode"]),"Price"]
  }
}

#We will do the same thing with the missing descriptions, as there are items
# that have missing NA values for one record that have a name for others
missingdesc = df[is.na(df$Description),"StockCode"]
for (i in missingdesc$StockCode) {
  temp = df[df$StockCode == as.character(i),"Description"]
  if (sum(!is.na(temp) > 0)) {
    df[df$StockCode == as.character(i),"Description"] = unique(na.omit(temp))[1,]
  }
}

#Removing just the negative quantity of items may affect the data integrity
# if there was also a record of positive quantity. So the better approach is to
# take the sum of the transactions (including transaction cancelled) and summing
# each item total together. If a customer bought two items and returned one,
# then that one item will be removed but the other item will stay.
bad_trans_item = data.frame("Invoice" = character(), "StockCode" = character())
k = 1
for (i in unique(df$Invoice)){
  transaction = df[grep(i,df$Invoice),]
  for (j in unique(transaction$StockCode)) {
    transaction_item = transaction[transaction$StockCode == j,]
    if (sum(transaction_item$Quantity) <= 0) {
      bad_trans_item[k,] = c(i,j)
      bad_trans_item[k+1,] = c(paste(i,"C",sep = ''),j)
      k = k+2
    }
  }
}

#Remove transaction/item combination with negative or 0 quantity
df = anti_join(df, bad_trans_item, by = c('Invoice','StockCode'))

#
sum(df$Price == 0)

#After all of the preprocessing done, lets look at the descriptions for the
# remaining items that have a $0 price, including how many are missing data
needlessItem = df[df$Price == 0,"Description"]
#only 67 more items and are marked as $0
dim(needlessItem)
#of the 67 items, 60 are missing descriptions
sum(is.na(needlessItem))
#What are 7 item descriptions left that are $0?
unique(needlessItem)

#Remove remaining items that have both a missing description and $0 price 
# as they do not add anything to the analysis
df = df[!(df$Price == 0 & is.na(df$Description)),]

#no more missing values for description!
colSums(is.na(df))

#country doesn't need many changes, just a couple of changes to make names
# a little more clearer
unique(df$Country)
df[df$Country == "EIRE","Country"] = "Ireland"
df[df$Country == "RSA","Country"] = "South Africa"

#Finally, lets look at our customer id data.
#Only 118 of the 5882 customer ids have only one transaction, so this gives 
# a lot of opportunity to see how transactions carry over to following trans.
df %>%
  select(Invoice, CustomerID) %>%
  group_by(CustomerID) %>%
  summarise(trans_count = n()) %>%
  group_by(trans_count) %>%
  summarise (n = n()) %>%
  arrange(trans_count)

df %>%
  select(Price) %>%
  group_by(Price) %>%
  summarise(n = n()) %>%
  arrange(n)

#There are a few adjustments as well, we can remove these as well as they don't
# apply much to the data (only 6 records)
df = anti_join(df,df[grep("A",df$Invoice),])

write.csv(df, file = "C:\\Users\\LBlac\\Downloads\\online_retailer_upd.csv")
