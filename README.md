---
title: "Grocery Store Coupon Campaign EDA"
author: "D'Ette Wagar"
date: "2024-05-12"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### This project explores a data set called "The Complete Journey," which was developed by Brad Boehmke and Steven M. Mortimer. It contains transaction level data from a group of 2,469 frequent shoppers at a grocery store. It includes all of the shopper's transactional data, along with their demographic information and information relating to marketing campaign information, including coupons they received and store promotions.

### I am interested in exploring how different coupon campaigns influence shoppers buying decisions.

```{r main}
# Load R packages
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
install.packages("tidyselect")
library(tidyselect)
install.packages("reshape2")
library(reshape2)
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("corrplot")
library(corrplot)
install.packages("completejourney")
library(completejourney)

install.packages("remotes")
remotes::install_github("bradleyboehmke/completejourney")

# Let's begin by just getting an idea of what is in the data.

# Get coupon data
coupons_df <- coupons
glimpse(coupons_df)

# Get product data
products_df <- products
glimpse(products_df)

# Get demographics data
demo_df <- demographics
glimpse(demo_df)

# Get tramsaction data
transactions_df <- get_transactions()
glimpse(transactions_df)
```

### Let's try to determine whether campaigns were successful in driving up sales in the products they promoted.

```{r campaign detail}

# Display information on coupon campaigns

coup_camp_df <- campaign_descriptions
print.data.frame(coup_camp_df)
```

### When the campaign type is B or C, the customer receives all of the corresponding coupons in the coupon table. My first 3 analyses will focus on campaigns of these types, namely: Campaigns 14, 10, and 16. 
<br>

## Campaign 14

### The first campaign I will examine is campaign 14. Since it ran from 9-4-2017 to 11-8-2017, I will be able to compare the transaction data on applicable products from during the campaign to data on the same products on periods before and after the campaign.

```{r campaign fourteen}

# Let's look at the coupon data for campaign ID 14

# We can see that there are number of different product_ids associated with
# the various coupon_upcs attached to this campaign
coup_df <- coupons_df %>%
  select(campaign_id, product_id, coupon_upc) %>%
  group_by(campaign_id, coupon_upc) %>%
  filter(campaign_id == 14) %>%
  tally()

print.data.frame(coup_df)

# Now, we want to see which households participated in this campaign. From 
# there, we will examine how this correlates to the spending of those 
# households both before and after the campaign.

# View purchases made by participating households during campaign 14
cam14_redem_df <- coupon_redemptions %>%
  select(campaign_id, household_id, coupon_upc, redemption_date) %>%
  filter(campaign_id == 14)

glimpse(cam14_redem_df)

# Let's see how many sale items each household purchased during the sale 
# period

# First, merge redemption data with coupon data
cam14_coup_df <- merge(coupons_df, cam14_redem_df, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by.x = c("household_id", "product_id"),
                       by.y = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, 
         redemption_date, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-09-04"),
           as.Date("2017-11-08")),
         between(as.Date(redemption_date), as.Date("2017-09-04"),
           as.Date("2017-11-08")))

# Create a data file of the household IDs and product IDs that definitely 
# have transactions within the campaign window. (Unfortunately, we cannot 
# assume that all coupons in the redemption table have matches in the 
# transaction table, and we want to make sure that we are not capturing 
# transactions for any households that did not participate in the campaign.) 
cam14_house_prod_df <- cam14_tran_df %>%
  distinct(household_id, product_id)

# Count the quantity of applicable products purchased during the campaign 
# period per household
sum_cam14_tran_df <- cam14_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity by determining quantity purchased per day; 
# that way, we can accurately compare sales from before, during, and 
# after the campaign period

# Get number of days in campaign
cam14_days <- as.numeric(difftime("2017-11-08", "2017-09-04", units = "days"))

print(cam14_days)

# Add the quantity per day field (norm_quantity) to the summary campaign 
# data frame and add field showing period relative to campaign
sum_cam14_tran_df <- sum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam14_days),
         per_rel_camp = "During",
         per_rel_num = 1)

print.data.frame(sum_cam14_tran_df)

# Merge the redemption/coupon data with the transaction data from BEFORE 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
pcam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) < "2017-09-04")

pcam14_tran_df <- merge(pcam14_tran_df, cam14_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased before the campaign 
# period per household
psum_cam14_tran_df <- pcam14_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# Normalize the quantity value for the period before the campaign
pcam14_days <- as.numeric(difftime("2017-09-04", "2017-01-01", units = "days"))

print(pcam14_days)

# Add field to the summary campaign data frame showing the normalized
# quantity value, and add field showing period relative to campaign
psum_cam14_tran_df <- psum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/pcam14_days),
         per_rel_camp = "Before",
         per_rel_num = 0)

print.data.frame(psum_cam14_tran_df)

# Merge the redemption/coupon data with the transaction data from AFTER 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
acam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) > "2017-11-08")

acam14_tran_df <- merge(acam14_tran_df, cam14_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased after the campaign 
# period per household
asum_cam14_tran_df <- acam14_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

print.data.frame(asum_cam14_tran_df)

# Normalize the quantity value for the period after the campaign
acam14_days <- as.numeric(difftime("2018-01-01", "2017-11-08", units = "days"))

print(acam14_days)

# Add field to the summary campaign data frame showing the normalized
# quantity value, and add field showing period relative to campaign
asum_cam14_tran_df <- asum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/acam14_days),
         per_rel_camp = "After",
         per_rel_num = 2)

print.data.frame(asum_cam14_tran_df)
```

### We have the rates at which the campaign products were purchased (quantity/day) for the periods before the campaign, during the campaign, and after the campaign. Now, let's merge that data and observe it in a bar chart. From there, we can see if there are any statistical correlations to be drawn.

```{r campaign fourteen analysis}

#Join campaign transaction data with data from before the campaign
all_cam14_tran_df <- rbind(sum_cam14_tran_df, psum_cam14_tran_df)

print.data.frame(all_cam14_tran_df)

#Join campaign transaction data with data from after the campaign
all_cam14_tran_df <- rbind(all_cam14_tran_df, asum_cam14_tran_df)

print.data.frame(all_cam14_tran_df)
```

### Display purchase rate for each household by time period relative to campaign period
```{r campaign fourteen plot, echo=FALSE}
library(ggplot2)
  ggplot(all_cam14_tran_df, aes(x=household_id, y=norm_quantity,
                                  fill=per_rel_camp)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Qualifying Purchases for 
       Periods Relating to Campaign Fourteen\n")+
  scale_fill_manual(name="Time Periods Relating\nto Campaign",
                       values = c("#66C2A5", "#3288BD", "#5E4FA2")) +
  xlab("Household ID")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r campaign fourteen stats}
# Now let's see if there is any relationship between the variables
# norm_quantity and per_rel_camp (using per_rel_id because it is numeric). 
# We will find the correlation and test the null hypothesis that the two values # are not related to one another.
cor.test(all_cam14_tran_df$norm_quantity,
         all_cam14_tran_df$per_rel_num)
```

### As the statistical analysis shows, there does not seem to be a correlation between the effects of the campaign and the spending habits of the households who purchased products included in the campaign, in the sense that households did not favor a time period (before, during, or after the campaign) to purchase campaign products. The correlation coefficient is .288, with values approaching 1 indicating the closest correlation. Furthermore, we can reject the hypothesis that there is NO correlation between the variables of quantity purchased and time period purchased (before, during, or after campaign) if the probablity (p value) is smaller than .05 (5%). We can see here that the probability is .123. It is not particularly high, but it is not low enough to reject the hypothesis that there is no correlation between the variables. Unfortunately, we have no statistical evidence that the rate of quantities purchased is related to the variable that we have defined as before, during, or after the campaign period.
<br>

## Campaign 10

### The second campaign I will examine is campaign 10. It took place more toward the middle of the year, running from 6-28-2017 to 7-30-2017.

```{r campaign ten}

# Let's look at the coupon data for campaign ID 10

# We can see that there are number of different product_ids associated with
# the various coupon_upcs attached to this campaign
coup_df <- coupons_df %>%
  select(campaign_id, product_id, coupon_upc) %>%
  group_by(campaign_id, coupon_upc) %>%
  filter(campaign_id == 10) %>%
  tally()

print.data.frame(coup_df)

# Now, we want to see which households participated in this campaign. From 
# there, we will examine how this correlates to the spending of those 
# households both before and after the campaign.

# View purchases made by participating households during campaign 10
cam10_redem_df <- coupon_redemptions %>%
  select(campaign_id, household_id, coupon_upc, redemption_date) %>%
  filter(campaign_id == 10)

glimpse(cam10_redem_df)

# Let's see how many sale items each household purchased during the sale 
# period

# First, merge redemption data with coupon data
cam10_coup_df <- merge(coupons_df, cam10_redem_df, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam10_tran_df <- merge(cam10_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity,
         redemption_date, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-06-28"),
           as.Date("2017-07-30")),
         between(as.Date(redemption_date), as.Date("2017-06-28"),
           as.Date("2017-07-30")))

# Create a data file of the household IDs and product IDs that definitely 
# have transactions within the campaign window. (Unfortunately, we cannot 
# assume that all coupons in the redemption table have matches in the 
# transaction table, and we want to make sure that we are not capturing 
# transactions for any households that did not participate in the campaign.) 
cam10_house_prod_df <- cam10_tran_df %>%
  distinct(household_id, product_id)

# Count the quantity of applicable products purchased during the campaign 
# period per household
sum_cam10_tran_df <- cam10_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity by determining quantity purchased per day; 
# that way, we can accurately compare sales from before, during, and 
# after the campaign period

# Get number of days in campaign
cam10_days <- as.numeric(difftime("2017-07-30", "2017-06-28", units = "days"))

print(cam10_days)

# Add the quantity per day field (norm_quantity) to the summary campaign 
# data frame and add field showing period relative to campaign
sum_cam10_tran_df <- sum_cam10_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam10_days),
         per_rel_camp = "During",
         per_rel_num = 1)

print.data.frame(sum_cam10_tran_df)

# Merge the redemption/coupon data with the transaction data from BEFORE 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
pcam10_tran_df <- merge(cam10_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) < "2017-06-28")

pcam10_tran_df <- merge(pcam10_tran_df, cam10_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased before the campaign 
# period per household
psum_cam10_tran_df <- pcam10_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# Normalize the quantity value for the period before the campaign
pcam10_days <- as.numeric(difftime("2017-06-28", "2017-01-01", units = "days"))

print(pcam10_days)

# Add field to the summary campaign data frame showing the normalized 
# quantity value, and add field showing period relative to campaign
psum_cam10_tran_df <- psum_cam10_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/pcam10_days),
         per_rel_camp = "Before",
         per_rel_num = 0)

print.data.frame(psum_cam10_tran_df)

# Merge the redemption/coupon data with the transaction data from AFTER 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
acam10_tran_df <- merge(cam10_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) > "2017-07-30")

acam10_tran_df <- merge(acam10_tran_df, cam10_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased after the campaign 
# period per household
asum_cam10_tran_df <- acam10_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

print.data.frame(asum_cam10_tran_df)

# Normalize the quantity value for the period after the campaign
acam10_days <- as.numeric(difftime("2018-01-01", "2017-07-30", units = "days"))

print(acam10_days)

# Add field to the summary campaign data frame showing the normalized 
# quantity value, and add field showing period relative to campaign
asum_cam10_tran_df <- asum_cam10_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/acam10_days),
         per_rel_camp = "After",
         per_rel_num = 2)

print.data.frame(asum_cam10_tran_df)
```

### We have the rates at which the campaign products were purchased (quantity/day) for the periods before the campaign, during the campaign, and after the campaign. Now, let's merge that data and observe it in a bar chart. From there, we can see if there are any statistical correlations to be drawn.

```{r campaign ten analysis}
# Join campaign transaction data with data from before the campaign
all_cam10_tran_df <- rbind(sum_cam10_tran_df, psum_cam10_tran_df)

print.data.frame(all_cam10_tran_df)

# Join campaign transaction data with data from after the campaign
all_cam10_tran_df <- rbind(all_cam10_tran_df, asum_cam10_tran_df)

print.data.frame(all_cam10_tran_df)
```

### Display purchase rate for each household by time period relative to campaign period
```{r campaign ten plot, echo=FALSE}
library(ggplot2)
  ggplot(all_cam10_tran_df, aes(x=household_id, y=norm_quantity,
                                  fill=per_rel_camp)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Qualifying Purchases for 
       Periods Relating to Campaign Ten\n")+
  scale_fill_manual(name="Time Periods Relating\nto Campaign",
                       values = c("#66C2A5", "#3288BD", "#5E4FA2")) +
  xlab("Household ID")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r campaign ten stats}
# Now let's see if there is any relationship between the variables
# norm_quantity and per_rel_camp (using per_rel_id because it is 
# numeric). We will find the correlation and test the null hypothesis 
# that the two values are not related to one another.
cor.test(all_cam10_tran_df$norm_quantity,
         all_cam10_tran_df$per_rel_num)
```

### As the statistical analysis shows, there also does not seem to be a correlation between the effects of campaign ten and the spending habits of the households who purchased products included in the campaign. In fact, there seems to be even less of a correlation than there was in campaign fourteen, and the p value of .44 indicates that we are a long way from being able to reject the hypothesis that there is no relationship between the variables. This result is not surprising, considering the fact that there are even fewer households involved in this campaign than there were in campaign 14 (i.e., the population is smaller). 
<br>

## Campaign 16

### The third campaign I will examine is campaign 16. It took place more toward the middle of the year, running from 10-4-2017 to 11-5-2017.

```{r campaign sixteen}

# Now, let's look at the coupon data for campaign ID 16

# We can see that there are number of different product_ids associated with
# the various coupon_upcs attached to this campaign
coup_df <- coupons_df %>%
  select(campaign_id, product_id, coupon_upc) %>%
  group_by(campaign_id, coupon_upc) %>%
  filter(campaign_id == 16) %>%
  tally()

print.data.frame(coup_df)

# We want to see which households participated in this campaign. From 
# there, we will examine how this correlates to the spending of those 
# households both before and after the campaign.

# View purchases made by participating households during campaign 16
cam16_redem_df <- coupon_redemptions %>%
  select(campaign_id, household_id, coupon_upc, redemption_date) %>%
  filter(campaign_id == 16)

glimpse(cam16_redem_df)

# Let's see how many sale items each household purchased during the sale 
# period

# First, merge redemption data with coupon data
cam16_coup_df <- merge(coupons_df, cam16_redem_df, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam16_tran_df <- merge(cam16_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, 
         redemption_date, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-10-04"),
           as.Date("2017-11-05")),
         between(as.Date(redemption_date), as.Date("2017-10-04"),
           as.Date("2017-11-05")))

# Create a data file of the household IDs and product IDs that definitely 
# have transactions within the campaign window. (Unfortunately, we cannot 
# assume that all coupons in the redemption table have matches in the 
# transaction table, and we want to make sure that we are not capturing 
# transactions for any households that did not participate in the campaign.) 
cam16_house_prod_df <- cam16_tran_df %>%
  distinct(household_id, product_id)

# Count the quantity of applicable products purchased during the campaign 
# period per household
sum_cam16_tran_df <- cam16_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity by determining quantity purchased per day; 
# that way, we can accurately compare sales from before, during, and 
# after the campaign period

# Get number of days in campaign
cam16_days <- as.numeric(difftime("2017-11-05", "2017-10-04", units = "days"))

print(cam16_days)

# Add the quantity per day field (norm_quantity) to the summary campaign 
# data frame and add field showing period relative to campaign
sum_cam16_tran_df <- sum_cam16_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam16_days),
         per_rel_camp = "During",
         per_rel_num = 1)

print.data.frame(sum_cam16_tran_df)

# Merge the redemption/coupon data with the transaction data from BEFORE 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
pcam16_tran_df <- merge(cam16_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) < "2017-10-04")

pcam16_tran_df <- merge(pcam16_tran_df, cam16_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased before the campaign 
# period per household
psum_cam16_tran_df <- pcam16_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# Normalize the quantity value for the period before the campaign
pcam16_days <- as.numeric(difftime("2017-10-04", "2017-01-01", units = "days"))

print(pcam16_days)

# Add field to the summary campaign data frame showing the normalized
# quantity value, and add field showing period relative to campaign
psum_cam16_tran_df <- psum_cam16_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/pcam16_days),
         per_rel_camp = "Before",
         per_rel_num = 0)

print.data.frame(psum_cam16_tran_df)

# Merge the redemption/coupon data with the transaction data from AFTER 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
acam16_tran_df <- merge(cam16_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) > "2017-11-05")

acam16_tran_df <- merge(acam16_tran_df, cam16_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased after the campaign 
# period per household
asum_cam16_tran_df <- acam16_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

print.data.frame(asum_cam16_tran_df)

# Normalize the quantity value for the period after the campaign
acam16_days <- as.numeric(difftime("2018-01-01", "2017-11-05", units = "days"))

print(acam16_days)

# Add field to the summary campaign data frame showing the normalized
# quantity value, and add field showing period relative to campaign
asum_cam16_tran_df <- asum_cam16_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/acam16_days),
         per_rel_camp = "After",
         per_rel_num = 2)

print.data.frame(asum_cam16_tran_df)
```

### We have the rates at which the campaign products were purchased (quantity/day) for the periods before the campaign, during the campaign, and after the campaign. Now, let's merge that data and observe it in a bar chart. From there, we can see if there are any statistical correlations to be drawn.

```{r campaign sixteen analysis}
#Join campaign transaction data with data from before the campaign
all_cam16_tran_df <- rbind(sum_cam16_tran_df, psum_cam16_tran_df)

print.data.frame(all_cam16_tran_df)

#Join campaign transaction data with data from after the campaign
all_cam16_tran_df <- rbind(all_cam16_tran_df, asum_cam16_tran_df)

print.data.frame(all_cam16_tran_df)
```

### Display purchase rate for each household by time period relative to campaign period
```{r campaign sixteen plot, echo=FALSE}
library(ggplot2)
  ggplot(all_cam16_tran_df, aes(x=household_id, y=norm_quantity,
                                  fill=per_rel_camp)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Qualifying Purchases for 
       Periods Relating to Campaign Sixteen\n")+
  scale_fill_manual(name="Time Periods Relating\nto Campaign",
                       values = c("#66C2A5", "#3288BD", "#5E4FA2")) +
  xlab("Household ID")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r campaign sixteen stats}
# Now let's see if there is any relationship between the variables
# norm_quantity and per_rel_camp (using per_rel_id because it is numeric). 
# We will find the correlation and test the null hypothesis that the two values # are not related to one another.
cor.test(all_cam16_tran_df$norm_quantity, all_cam16_tran_df$per_rel_num)
```

### As the statistical analysis shows, there once again does not seem to be a correlation between the effects of campaign ten and the spending habits of the households who purchased products included in the campaign. The correlation coefficient is low at .18 and the p value is relatively high at .41. We clearly cannot reject the hypothesis that there is no correlation between the variables of quantity purchased and time period purchased. Unfortunately, we have no evidence that the quantity of qualifying products purchased correlates with the variable corresponding to the campaign period.

<br>

## Campaign 8

### The third campaign I will examine is campaign 16. It took place more toward the middle of the year, running from 5-8-2017 to 6-25-2017. Unlike the other campaigns examined, campaign 8 is an example of a type A campaign. As such, the households receive a subset of the coupons associated with the campaign, but not all of them, as they would in campaigns of type B or C. (The exact coupons associated with the households are not indicated in the data set.)

```{r campaign eight}

# Let's look at the coupon data for campaign ID 8 (Type A)

# We can see that there are number of different product_ids associated with
# the various coupon_upcs attached to this campaign
coup_df <- coupons_df %>%
  select(campaign_id, product_id, coupon_upc) %>%
  group_by(campaign_id, coupon_upc) %>%
  filter(campaign_id == 8) %>%
  tally()

print.data.frame(coup_df)

# Now, we want to see which households participated in this campaign. From 
# there, we will examine how this correlates to the spending of those 
# households both before and after the campaign.

# View purchases made by participating households during campaign 8
cam8_redem_df <- coupon_redemptions %>%
  select(campaign_id, household_id, coupon_upc, redemption_date) %>%
  filter(campaign_id == 8)

glimpse(cam8_redem_df)

# Let's see how many sale items each household purchased during the sale 
# period

# First, merge redemption data with coupon data
cam8_coup_df <- merge(coupons_df, cam8_redem_df, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam8_tran_df <- merge(cam8_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, 
         redemption_date, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-05-08"),
           as.Date("2017-06-25")),
         between(as.Date(redemption_date), as.Date("2017-05-08"),
           as.Date("2017-06-25")))

# Create a data file of the household IDs and product IDs that definitely 
# have transactions within the campaign window. (Unfortunately, we cannot 
# assume that all coupons in the redemption table have matches in the 
# transaction table, and we want to make sure that we are not capturing 
# transactions for any households that did not participate in the campaign.) 
cam8_house_prod_df <- cam8_tran_df %>%
  distinct(household_id, product_id)

# Count the quantity of applicable products purchased during the campaign 
# period per household
sum_cam8_tran_df <- cam8_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity by determining quantity purchased per day; 
# that way, we can accurately compare sales from before, during, and 
# after the campaign period

# Get number of days in campaign
cam8_days <- as.numeric(difftime("2017-06-25", "2017-05-08", units = "days"))

print(cam8_days)

# Add the quantity per day field (norm_quantity) to the summary campaign 
# data frame and add field showing period relative to campaign
sum_cam8_tran_df <- sum_cam8_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam8_days),
         per_rel_camp = "During",
         per_rel_num = 1)


# Show the quantity purchased and the quantity purchased per week by
# household # during the campaign period. Show only a portion of this data, 
# though; since the campaign type is A, there are 209 coupon upc codes 
# associated with # this campaign, as compared to 26, 14, and 13 coupon 
# upc codes in the previous 3 campaigns, respectively.
glimpse(sum_cam8_tran_df)

# Merge the redemption/coupon data with the transaction data from BEFORE 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
pcam8_tran_df <- merge(cam8_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) < "2017-05-08")

pcam8_tran_df <- merge(pcam8_tran_df, cam8_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased before the campaign 
# period per household
psum_cam8_tran_df <- pcam8_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# Normalize the quantity value for the period before the campaign
pcam8_days <- as.numeric(difftime("2017-05-08", "2017-01-01", units = "days"))

print(pcam8_days)

# Add field to the summary campaign data frame showing the normalized
# quantity value, and add field showing period relative to campaign
psum_cam8_tran_df <- psum_cam8_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/pcam8_days),
         per_rel_camp = "Before",
         per_rel_num = 0)

# Again, view only a subset of the data from before the campaign period
glimpse(psum_cam8_tran_df)

# Merge the redemption/coupon data with the transaction data from AFTER 
# the campaign period, and then merge that with the table of household 
# IDs/product IDs that are participating in the campaign
acam8_tran_df <- merge(cam8_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) > "2017-06-25")

acam8_tran_df <- merge(acam8_tran_df, cam8_house_prod_df,
                        by = c("household_id", "product_id"))

# Count the quantity of applicable products purchased after the campaign 
# period per household
asum_cam8_tran_df <- acam8_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# View a subset of the data from after the campaign period
glimpse(asum_cam8_tran_df)

# Normalize the quantity value for the period after the campaign
acam8_days <- as.numeric(difftime("2018-01-01", "2017-06-25", units = "days"))

print(acam8_days)

# Add field to the summary campaign data frame showing the normalized quantity
# value, and add field showing period relative to campaign
asum_cam8_tran_df <- asum_cam8_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/acam8_days),
         per_rel_camp = "After",
         per_rel_num = 2)

# View only a subset of the data from after the campaign period
glimpse(asum_cam8_tran_df)
```

### We have the rates at which the campaign products were purchased (quantity/day) for the periods before the campaign, during the campaign, and after the campaign. Now, let's merge that data and observe it in a bar chart. From there, we can see if there are any statistical correlations to be drawn.

```{r campaign eight analysis}

#Join campaign transaction data with data from before the campaign
all_cam8_tran_df <- rbind(sum_cam8_tran_df, psum_cam8_tran_df)

#View a subset of the combined data
glimpse(all_cam8_tran_df)

#Join campaign transaction data with data from after the campaign
all_cam8_tran_df <- rbind(all_cam8_tran_df, asum_cam8_tran_df)

#View a subset of the combined data
glimpse(all_cam8_tran_df)
```

### Display purchase rate for each household by time period relative to campaign period
```{r quantity per week/household eight plot, echo=FALSE}
library(ggplot2)
  ggplot(all_cam8_tran_df, aes(x=household_id, y=norm_quantity,
                                  fill=per_rel_camp)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Qualifying Purchases for 
       Periods Relating to Campaign Eight\n")+
  scale_fill_manual(name="Time Periods Relating\nto Campaign",
                       values = c("#66C2A5", "#3288BD", "#5E4FA2")) +
  xlab("Household ID")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))

# Now let's see if there is any relationship between the variables
# norm_quantity and per_rel_camp (using per_rel_id because it is numeric). 
# We will find the correlation and test the null hypothesis that the two 
# values are not related to one another.
cor.test(all_cam8_tran_df$norm_quantity, all_cam8_tran_df$per_rel_num)
```

### The statistical analysis shows, as with the other campaigns, there does not seem to be a correlation between rate of purchase for the item(s) targeted in the campaign and the campaign period. The correlation coefficient is very low (-.03), and the we would need a p value of less than .05 to reject the hypothesis that there is NO relationship between the variables. Our p variable is .51. There are undoubtedly many other variables at play here, but I have not touched upon the ones that are correlating with one another.
<br>

### Since examining quantities of campaign products in relation to when they were purchased did not tell us much, let's now look at demographic data related to the campaign products that were purchased during the campaign period. Although we do not have household data on all households (and therefore linking to households will limit the transactions data), that is an acceptable constraint since we want to examine household information in relation to transactional data. Specifically, we want to see if the quantity of sales items purchased has any statistically significant relationship to any demographic variables.

```{r eight household}
# Build table of households/purchases again (because previous 
# data frames were so heavily manipulated )

# View purchases made by participating households during campaign 8
cam8_redem_df2 <- coupon_redemptions %>%
  select(campaign_id, household_id, coupon_upc, redemption_date) %>%
  filter(campaign_id == 8)

glimpse(cam8_redem_df2)

# Let's see how many sale items each household purchased during the 
# sale period

# First, merge redemption data with coupon data
cam8_coup_df2 <- merge(coupons_df, cam8_redem_df2, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam8_tran_df2 <- merge(cam8_coup_df2, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, 
           redemption_date, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-05-08"),
           as.Date("2017-06-25")),
         between(as.Date(redemption_date), as.Date("2017-05-08"),
           as.Date("2017-06-25")))

# Count the quantity of applicable products purchased during the 
# campaign period per household
sum_cam8_tran_df2 <- cam8_tran_df2 %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity by determining quantity purchased per day; 
# that way, we can accurately compare sales from before, during, and 
# after the campaign period

cam8_days2 <- as.numeric(difftime("2017-06-25", "2017-05-08", units = "days"))

# Add field to the summary campaign data frame showing the normalized quantity
# value (which is quantity/days of period)
sum_cam8_tran_df2 <- sum_cam8_tran_df2 %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam8_days2))


# Now that the household/products/transactions table has been rebuilt, 
# join to demographics data
cam8_sum_hous_tran_df <- merge(sum_cam8_tran_df2, demo_df, 
                                by = "household_id")

glimpse(cam8_sum_hous_tran_df)
```

### Let's aggregate Campaign 8 data by age and income. 

### Find the average quantity of product sold during the campaign, as well as the average daily sal rate, by: 1) age, 2) income, and 3) age and income

``` {r household campaign eight age}
demo_df %>% distinct(age)
demo_df %>% distinct(income)

#Age

# Get the count of households, average quantity, and average 
# daily quantity by age range
cam8_age_df <- cam8_sum_hous_tran_df %>%
  select(age, tot_quantity, norm_quantity, household_id) %>%
  mutate(house_ct = 1) %>%
  group_by(age) %>%
  summarize(avg_quant = mean(tot_quantity),
            avg_norm_quant = mean(norm_quantity),
            house_ct = sum(house_ct))
```

```{r household campaign eight age plot, echo=FALSE}
library(ggplot2)
  ggplot(cam8_age_df, aes(x=age, y=avg_norm_quant)) +
  geom_bar(stat="identity",position="dodge", fill="#66C2A5")+
  labs(title="Qualifying Purchases as They Relate to Age")+
  xlab("Household Age")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r household campaign eight age stats}
# Let's look at statistics for age

# Get the total count of households
tot_house_age <- sum(cam8_age_df$house_ct)

print(tot_house_age)

# Display age range, average quanity, average daily quantity, and
# percentage of households
trunc_cam8_age_df <- cam8_age_df %>%
  select(age, avg_quant, avg_norm_quant, house_ct) %>%
  mutate(house_pct = house_ct/tot_house_age)

print.data.frame(trunc_cam8_age_df)

# Is there a statistical correlation between percentage of households 
# at a specific age range and the average quantity per week purchased 
# (product included in campaign)?
cor.test(as.numeric(trunc_cam8_age_df$house_pct),
         as.numeric(trunc_cam8_age_df$avg_norm_quant))

```

### There is not a true statistical correlation between the percentage of households at a specific age range and the average quantity per week of qualifying product(s), although the correlation coefficient is .634. The p value would have to be less than .05 to reject the hypothesis that the two values are not correlated at all, and the p value is higher than that, at .177. **Looking at these values and the bar graph, however, would lead me to target my marketing dollars on shoppers under 65 years of age.**

``` {r household campaign eight income}
#Income

# Get the count of households, average quantity per week, and average 
# quantity by income range
cam8_inc_df <- cam8_sum_hous_tran_df %>%
  select(income, tot_quantity, norm_quantity, household_id) %>%
  mutate(house_ct = 1,
         income_rank = case_when(income == "Under 15K" ~ 0,
                                 income == "15-24K" ~ 15, 
                                 income == "25-34K" ~ 25,
                                 income == "35-49K" ~ 35,
                                 income == "50-74K" ~ 50,
                                 income == "75-99K" ~ 75,
                                 income == "100-124K" ~ 100,
                                 income == "125-149K" ~ 125,
                                 income == "150-174K" ~ 150,
                                 income == "175-199K" ~ 175,
                                 income == "250+" ~ 250)) %>%
  group_by(income, income_rank) %>%
  summarize(avg_quant = mean(tot_quantity),
            avg_norm_quant = mean(norm_quantity),
            house_ct = sum(house_ct))
```

```{r household campaign eight income plot, echo=FALSE}

library(ggplot2)
  ggplot(cam8_inc_df, aes(x=income, y=avg_norm_quant)) +
  geom_bar(stat="identity",position="dodge", fill="#3288BD")+
  labs(title="Qualifying Purchases as They Relate to Income")+
  xlab("Income")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r household campaign eight income stats}
# Let's look at statistics for income

# Get the total count of households
tot_house <- sum(cam8_inc_df$house_ct)
  
print(tot_house)

# Display income, average quanity, average quantity per week, and
# percentage of households
trunc_cam8_inc_df <- cam8_inc_df %>%
  select(income, avg_quant, avg_norm_quant, house_ct) %>%
  mutate(house_pct = house_ct/tot_house)
  
print.data.frame(trunc_cam8_inc_df)

# Is there a statistical correlation between percentage of households 
# at a specific income level and the average quantity per week 
# purchased (product included in campaign)?
cor.test(as.numeric(trunc_cam8_inc_df$house_pct),
         as.numeric(trunc_cam8_inc_df$avg_norm_quant))
```

### The results of this statistical analysis say that no, there is no inherent correlation between the percent of households at a particular income level and the quantity of qualifying products that they would purchase. As the income was analyzed here, the various values were unranked, with no meaning behind them. They could have been values like A, B, or C, insead of 25-34K, 35-49K, or 50-74K. In other words, households at income level C were no more likely to make fewer or more qualifying purchases than households at income level B, perhaps because there was no *meanings* or *rankings* tied to the analysis of those incomes.
```{r household campaign eight income/age}
# Income/Age

# Get the count of households, average quantity per week, average 
# quantity by age group and income group
cam8_inc_age_df <- cam8_sum_hous_tran_df %>%
  select(age, income, tot_quantity, norm_quantity, household_id) %>%
  mutate(house_ct = 1) %>%
  group_by(age, income) %>%
  summarize(avg_quant = mean(tot_quantity),
            avg_norm_quant = mean(norm_quantity),
            house_ct = sum(house_ct))
```

```{r household campaign eight income/age plot, echo=FALSE}
library(ggplot2)
  ggplot(cam8_inc_age_df, aes(x=income, y=avg_norm_quant,
                                  fill=age)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Qualifying Purchases as They Relate to\n
       Age and Income\n")+
  scale_fill_manual(name="Head of Household Age",
                       values = c("#F46D43", "#FDAE61", "#FFFFBF","#66C2A5",
                                  "#3288BD", "#5E4FA2")) +
  xlab("Household Income")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))
```

```{r household campaign eight age/income stats}
# Get the count of households, average quantity per week, average 
# quantity, and average income rank by age group
cam8_inc_age_df2 <- cam8_sum_hous_tran_df %>%
  select(age, income, tot_quantity, norm_quantity, household_id) %>%
  mutate(house_ct = 1,
    income_rank = case_when(income == "Under 15K" ~ 0,
                                 income == "15-24K" ~ 15, 
                                 income == "25-34K" ~ 25,
                                 income == "35-49K" ~ 35,
                                 income == "50-74K" ~ 50,
                                 income == "75-99K" ~ 75,
                                 income == "100-124K" ~ 100,
                                 income == "125-149K" ~ 125,
                                 income == "150-174K" ~ 150,
                                 income == "175-199K" ~ 175,
                                 income == "200-249K" ~ 200,
                                 income == "250+" ~ 250)) %>%
  group_by(age) %>%
  summarize(avg_quant = mean(tot_quantity),
            avg_norm_quant = mean(norm_quantity),
            avg_inc_rank = mean(income_rank),
            house_ct = sum(house_ct))

glimpse(cam8_inc_age_df2)

# Let's look at statistics for age/income

# Get the total count of households
tot_house <- sum(cam8_inc_df$house_ct)

print(tot_house)

# Display average income rank, average quanity, average quantity per
# week, and percentage of households
trunc_cam8_inc_age_df <- cam8_inc_age_df2 %>%
  select(avg_inc_rank, avg_quant, avg_norm_quant, house_ct) %>%
  mutate(house_pct = house_ct/tot_house)
  
print.data.frame(trunc_cam8_inc_age_df)

# Is there a statistical correlation between average income rank at a 
# specific age and the average quantity per week purchased (product
# included in campaign)?
cor.test(as.numeric(trunc_cam8_inc_age_df$avg_inc_rank),
         as.numeric(trunc_cam8_inc_age_df$avg_norm_quant))
```

### The answers to this question is no, based on the data; the correlation coefficient between the average income rank in a certain age range and the average quantity of qualifying purchases is .773, with a p value of .125. As the bar graphs show, and the data in the table below shows, this campaign was a hit with 2 households in the $250,000+ income and with one household in particular that fell in the $25,000 - $34,000 income range and also in the 55 to 64 year age range.

```{r exploration}
# Select count of households, average quantity purchased, and average 
# quantity per week purchased by age range and income range for products
# included in campaign 8
cam8_age_df1 <- cam8_sum_hous_tran_df %>%
  select(age, income, tot_quantity, norm_quantity, household_id) %>%
  mutate(house_ct = 1) %>%
  group_by(age, income) %>%
  summarize(avg_quant = mean(tot_quantity),
            avg_norm_quant = mean(norm_quantity),
            house_ct = sum(house_ct))

print.data.frame(cam8_age_df1)

```
