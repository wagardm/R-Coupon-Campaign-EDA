---
title: "Marketing Results EDA"
author: "D'Ette Wagar"
date: "2024-05-12"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### This project explores a data set called "The Complete Journey," which was developed by Brad Boehmke and Steven M. Mortimer. It contains transaction level data from a group of 2,469 frequent shoppers at a grocery store. It includes all of the shopper's transactional data, along with their demographic information and information relating to marketing campaign information, inluding coupons they received and store promotions.

### I am interested in exploring how different coupon campaigns and/or campaigns influence shoppers buying decisions.

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

# Get tramsactopm data
transactions_df <- get_transactions()
glimpse(transactions_df)
```

### Let's try to determine whether campaigns were successful in driving up sales in the products they promoted.

```{r campaign detail}

# Display information on coupon campaigns

coup_camp_df <- campaign_descriptions
print.data.frame(coup_camp_df)
```

### When the campaign type is B or C, the customer receives all of the corresponding coupons in the coupon table. Therefore, further analysis will focus on campaigns of these types. 

### The first campaign I will examine is campaign 14; since it ran from 9-4-2017 to 11-8-2017, I will be able to compare the transaction data from a couple of months before and after the campaign to the data during the campaign.

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

# Let's see how many sale items each household purchased during the sale # period

# First, merge redemption data with coupon data
cam14_coup_df <- merge(coupons_df, cam14_redem_df, 
                       by = c("campaign_id","coupon_upc"))

# Next, merge redemption/coupon data with transaction data from the 
# campaign period
cam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(between(as.Date(transaction_timestamp), as.Date("2017-09-04"),
           as.Date("2017-11-08")))

# Count the quantity of applicable products purchased during the campaign 
# period per household
sum_cam14_tran_df <- cam14_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))
  
# Normalize the quantity in proportion to the portion of the year that the 
# campaign period is; that way, we can accurately compare sales from 
# before, during, and after the campaign period

cam14_days <- as.numeric(difftime("2017-11-08", "2017-09-04", units = "days"))

print(cam14_days)

# Calculate percent of year in campaign period
#cam14_perc = as.numeric(cam14_days/365)

#print(cam14_perc)

# Add field to the summary campaign data frame showing the normalized quantity
# value (which is quantity/days of period), and add field showing period 
# relative to campaign
sum_cam14_tran_df <- sum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/cam14_days),
         per_rel_camp = "DURING",
         per_rel_num = 1)

print.data.frame(sum_cam14_tran_df)

# Merge the redemption/coupon data with the transaction data from BEFORE 
# the campaign period
pcam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) < "2017-09-04")

# Count the quantity of applicable products purchased before the campaign 
# period per household
psum_cam14_tran_df <- pcam14_tran_df %>%
  select(household_id, quantity) %>%
  group_by(household_id) %>%
  summarize(tot_quantity = sum(quantity))

# Normalize the quantity value for the period before the campaign
pcam14_days <- as.numeric(difftime("2017-09-04", "2017-01-01", units = "days"))

print(pcam14_days)

# Calculate percent of year before the campaign period
#pcam14_perc = as.numeric(pcam14_days/365)

#print(pcam14_perc)

# Add field to the summary campaign data frame showing the normalized quantity # value, and add field showing period relative to campaign
psum_cam14_tran_df <- psum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/pcam14_days),
         per_rel_camp = "BEFORE",
         per_rel_num = 0)

print.data.frame(psum_cam14_tran_df)

# Merge the redemption/coupon data with the transaction data from AFTER 
# the campaign period
acam14_tran_df <- merge(cam14_coup_df, transactions_df, 
                       by = c("household_id", "product_id")) %>%
  select(household_id, product_id, quantity, transaction_timestamp) %>%
  filter(as.Date(transaction_timestamp) > "2017-11-08")

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

# Calculate percent of year after the campaign period
#acam14_perc = as.numeric(acam14_days/365)

#print(acam14_perc)

# Add field to the summary campaign data frame showing the normalized quantity
# value, and add field showing period relative to campaign
asum_cam14_tran_df <- asum_cam14_tran_df %>%
  select(household_id, tot_quantity) %>%
  mutate(norm_quantity = (tot_quantity/acam14_days),
         per_rel_camp = "AFTER",
         per_rel_num = 2)

print.data.frame(asum_cam14_tran_df)
```

### We have the rates at which the campaign products were purchased (quantity/day) for the periods before the campaign, during the campaign, and after the campaign. Now, let's merge that data and observe it in a bar chart. From there, we can see if there are any statistical correlations to be drawn.

```{r campaign fourteen analysis}

#Join campaign transaction data with data from before the campaign
all_cam14_tran_df <- rbind(sum_cam14_tran_df, psum_cam14_tran_df)

print.data.frame(all_cam14_tran_df)

#Join campaign transaction data with data from before the campaign
all_cam14_tran_df <- rbind(all_cam14_tran_df, asum_cam14_tran_df)

print.data.frame(all_cam14_tran_df)
```

### Display purchase rate for each household by time period relative to campaign period

```{r}
all_cam14_tran_df <- all_cam14_tran_df
#%>%
#  filter(household_id == "1142" | household_id == "472" | household_id == #"888")
#%>%
#  group_by(household_id) %>%
#  summarize(norm_quantity = mean(norm_quantity))

#all_cam14_tran_df.long <- melt(all_cam14_tran_df,id.vars="household_id")

brewer.pal(11, "Spectral")
brewer.pal(11, "PuBuGn")
#brewer.pal(11, "RedYlBu")

display.brewer.pal(11, "Spectral")
display.brewer.pal(11, "PuBuGn")
#display.brewer.pal(11, "RedYlBu")


```

```{r quantity per week/household plot, echo=FALSE}

library(ggplot2)
  ggplot(all_cam14_tran_df, aes(x=household_id, y=norm_quantity,
                                  fill=per_rel_camp)) +
  geom_bar(stat="identity",position="dodge")+
  labs(title="Comparison of Qualifying Purchases for 
       Periods Relating to Campaign Fourteen\n")+
  scale_fill_manual(name="Time Periods Relating\nto Campaign", 
                       labels = c("After", "Before", "During"),
                       values = c("#66C2A5", "#3288BD", "#5E4FA2")) +
#                      breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#                               15, 16, 17),
#                      labels=c("1142", "127", "1529", "1764", "2488", "2489", 
#                               "256", "464", "472", "560", "588", "600",
#                                "636", "641", "88", "888", "982"),
#                      values=c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61",
#                               "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4",
#                               "#66C2A5", "#3288BD", "#5E4FA2", "#A6BDDB",
#                               "#67A9CF", "#3690C0", "#02818A", "#016C59",
#                              "#014636"))+
  xlab("Household ID")+ylab("Quantity per Week")+
  theme(axis.text.x = element_text(angle = 45))

# Now let's see if there is any relationship between the variables
# norm_quantity and per_rel_camp (using per_rel_id because it is numeric). 
# We will find the correlation and test the null hypothesis that the two values # are not related to one another.
cor.test(all_cam14_tran_df$norm_quantity,
         all_cam14_tran_df$per_rel_num)
```
### As the statistical analysis shows, there does not seem to be a correlation between the effects of the campaign and the spending habits of the households who purchased products included in the campaign, in the sense that households did not favor a time period (before, during, or after the campaign) to purchase campaign products. The correlation coefficient is a mere .04, with values approaching 1 indicating the closest correlation. Furthermore, we can reject the hypothesis that there is NO correlation between the variables of quantity purchased and time period purchased (before, during, or after campaign) if the probablity (p value) is smaller than .05 (5%). We can see here that the probability is very large: 80%. Unfortunately, we have no evidence of a successful campaign.


