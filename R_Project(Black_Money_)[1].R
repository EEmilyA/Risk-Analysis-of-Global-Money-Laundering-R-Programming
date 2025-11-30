library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
#import data to R

Black_Money = read_csv("C:/Users/user/Desktop/Big_Black_Money.csv")

#cleaning functions:

#check missing and null values

summary(Black_Money)
Black_Money <- na.omit(Black_Money)
sapply(data, is.null)


#getting the structure of the data
str(Black_Money)
View(Black_Money)

#retrieving column names
colnames(Black_Money)

#retrieve only the first 6 rows
head(Black_Money)
#retrieve only the first 10 rows
tibble(Black_Money)

#Data Manipulation with visualization

# Drop columns
Black_Money <- Black_Money %>%
  select(- `Financial Institution`,-`Person Involved`)


# change to lowercase
rename_with(Black_Money,tolower)


# Rename columns
Black_Money <- Black_Money %>%
  rename(
    ID = `Transaction ID`,
    Usd_Amount = `Amount (USD)`,Date = `Date of Transaction`,
    Risk_Score = `Money Laundering Risk Score`,
    Shell_Company = `Shell Companies Involved`,
    Status_Report= `Reported by Authority`)


#Add a year column and change format from character to number
Black_Money$Date <- as.Date(Black_Money$Date)
Black_Money <- Black_Money %>%
  mutate(Year = year(Date))



#Data filtering


#total,average,max and min transactions by industry

Black_Money%>%
  group_by(Industry)%>%
  summarise(avg_Amt_spent = mean(Usd_Amount),
            total_amt=sum(Usd_Amount),
            max_amt= max(Usd_Amount),
            min_amt= min(Usd_Amount)) %>%
  arrange(desc(total_amt))


#total,average,max and min transactions by Destination COUNTRY

Black_Money%>%
  group_by(`Destination Country`)%>%
  summarise(avg_Amt_spent = mean(Usd_Amount),
            total_amt=sum(Usd_Amount),
            max_amt= max(Usd_Amount),
            min_amt= min(Usd_Amount))%>%
  arrange(desc(total_amt))

#Visualization of Total_amount spent by country
ggplot(Black_Money, aes(x = reorder(Country,Usd_Amount),
                        y = Usd_Amount / 1e6, fill = Industry)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top Countries at Risk for Money Laundering",
    x = "Country",
    y = "Total Amount (in Millions USD)"
  ) +
  theme_minimal()

#  transaction count by country and Industry

activity_summary<-Black_Money %>%
  group_by(Industry, Country) %>%
  summarise(
    Total_Transactions = n(),
    Amount = mean(Usd_Amount),
    .groups = 'drop'
  ) %>%
  arrange(desc(Amount))
print(activity_summary)


# Filter for suspicious transactions

laundry_status_report <- Black_Money %>%
  filter(Status_Report == TRUE) %>%
  group_by(`Transaction Type`) %>%
  summarise(Total_Cases = n(), .groups = 'drop')
print(laundry_status_report)


# Filter for illegal transactions and group by Country and Industry
Black_Money%>%
  filter(`Source of Money` == "Illegal") %>%
  group_by(Country, Industry,) %>%
  summarise(
    Total_Illegal_transactions = n(),
    amount_spent =mean(Usd_Amount) )
print(Black_Money)


# Summarize risk and transaction data by Tax Haven country
country_risk <- Black_Money %>%
  group_by(`Tax Haven Country`) %>%
  summarise(
    Avg_Risk_Score = mean(Risk_Score, na.rm = TRUE),
    Total_Transactions = n(),
    Total_Amount = sum(`Usd_Amount`, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Risk_Score))
print(country_risk)

# visualization of Tax Haven countries Using Scatter plot
ggplot(country_risk, aes(x = Total_Amount/1e4, y = Avg_Risk_Score)) +
  geom_point(color = "blue", size = 3)+
  geom_text(aes(label = `Tax Haven Country`), vjust = -0.3, size = 3) +
  labs(
    title = "TaxHavenCountry Risk vs Transaction Amount",
    x = "Amount (USD Million)",
    y = "Average Risk Score"
  ) +
  theme_minimal()



# Filter for high-risk transactions and Group by Destination Country
high_riskscore <- Black_Money %>%
  filter( Risk_Score > 7& Usd_Amount >=100000 ) %>%
  group_by(`Destination Country`) %>%
  summarise(Total_Transactions = n(),
            Total_Amount = sum(Usd_Amount, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(desc(Total_Transactions))
print(high_riskscore)


# Visualization:Risk score by Destination country and transaction type

ggplot(high_riskscore, aes(x = reorder(`Destination Country`, -Total_Transactions),
                           y = Total_Transactions, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(
    title = "High-Risk Transactions by Destination Country",
    x = "Destination Country",
    y = "Total Transactions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Summarize average risk score by industry
industry_risk <- Black_Money %>%
  group_by(Industry) %>%
  summarise(
    Avg_Risk_Score = mean(Risk_Score, na.rm = TRUE),
    Total_Transactions = n(),
    Total_Amount = sum(`Usd_Amount`, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Risk_Score))
print(industry_risk)


# Industry risk bar plot
ggplot(industry_risk, aes(x = reorder(Industry, Avg_Risk_Score),
                          y = Avg_Risk_Score, fill = Industry)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Risk Score by Industry",
       x = "Industry",
       y = "Average Risk Score") +
  theme_minimal()


# Filter for transactions with valid groups
legal_illegal_data <- Black_Money%>%
  filter(`Source of Money` %in% c("Legal", "Illegal"))


# Filter data for USA and years 2013 and 2014
Yearly_risktrend <- Black_Money %>%
  filter(Country == "USA" & Year %in% c("2013", "2014")) %>%
  arrange(Year)
Yearly_risktrend <- Yearly_risktrend %>%
  mutate(Year = as.factor(Year))


# yearly trends across industry and country
Yearly_risktrend %>%
  group_by(Year, Country, Industry) %>%
  summarise(
    `Transaction Type` = n(),
    .groups = "drop"
  )



#Advance categorical  variables

# two sample t-test

t.test(`Usd_Amount` ~ `Source of Money`, data = Black_Money)
var.test(Usd_Amount ~ `Source of Money`, data = Black_Money)



# Chi-square test between Industry and Risk
chisq.test(table(Black_Money$Industry, Black_Money$Risk_Score > 5))


# Industry vs. Suspicious transactions
chisq.test(table(Black_Money$Industry, Black_Money$Status_Report))


# Perform an independent t-test
t.test(
  x = Yearly_risktrend$Risk_Score[Yearly_risktrend$Year == "2013"],
  y = Yearly_risktrend$Risk_Score[Yearly_risktrend$Year == "2014"],
  paired = FALSE
)
var.test(Risk_Score ~ Year, data = Yearly_risktrend)

#Visualization
ggplot(Yearly_risktrend, aes(x = Year, y = Risk_Score, fill = Year)) +
  geom_boxplot() +
  labs(
    title = "Risk Score Distribution for 2013 and 2014",
    x = "Year",
    y = "Risk Score"
  ) +
  theme_minimal()


# correlation between Money Laundering Risk Score and Amount (USD)

cor.test(Black_Money$Usd_Amount, Black_Money$Risk_Score, method = "pearson")


#Can Money Laundering Risk Score be predicted by transaction features
model <- lm(Risk_Score ~ Usd_Amount + Industry, data = Black_Money)
summary(model)


#What factors predict whether a transaction is flagged (Reported by Authority)

Prediction_model <- glm( Status_Report ~ Risk_Score + Shell_Company + Usd_Amount,
                         data = Yearly_risktrend,
                         family = binomial(link = "logit")
)
summary(Prediction_model)


flagged_data <- Black_Money %>%
  filter(`Status_Report` == "TRUE")

# Perform a one-sample t-test
threshold_t_test <- t.test(
  flagged_data$Usd_Amount,
  mu = 1e6
)




