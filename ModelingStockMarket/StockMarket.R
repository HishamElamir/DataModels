##  STEP ONE
# INSTALL PACKAGES
install.packages("XML")
install.packages("reshape2")
install.packages("plyr")
install.packages("ggplot2")
install.packages("zoo")
# LOAD PACKAGES, DATA, AND CHECKING IT
library(XML)
library(reshape2)
library(plyr)
library(ggplot2)
library(zoo)

# SET SESSION PATH
setwd("D:/Master/Projects/DataScience/R/ModelingStockMarket/")

# LOAD AND VIEW DATASET
finviz <- read.csv("finviz.csv")
head(finviz)
summary(finviz)


# CLEANING NUMERIC DATA FUNCTION
clean_numeric_data <- function(s) {
  s <- gsub("%|\\$|,|\\)|\\(", "", s)
  s <- as.numeric(s)
}

# CLEAN DATA
finviz <- cbind(finviz[, 1:6], apply(finviz[, 7:68], 2, clean_numeric_data))
head(finviz)

# HISTOGRAMING DATA
hist(finviz$Price, breaks = 100, main = "Price Distribution", xlab = "Price") # Something Wrong (OUTLIER)
hist(finviz$Price[finviz$Price < 150], breaks = 100, main = "Price Distribution", xlab = "Price")

# GETTING SECTOR AVERAGE PRICES AND COMPARE THEM
sector_avg_price <- aggregate(Price~Sector, data = finviz, FUN = "mean")
colnames(sector_avg_price)[2] <- "Sector_Avg_Price"

ggplot(sector_avg_price, aes(x = Sector, y = Sector_Avg_Price, fill = Sector)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Sector Average Price")
# IT THEMES THAT FINANCIAL SECTOR HAS A VERY HIGH AVERAGE PRICE COMPARING TO OTHERS

# FIND OUT COMPANIES RESPONSIBLE FOR THIS AVERAGE PRICE
industry_avg_price <- aggregate(Price~Sector+Industry, data = finviz, FUN = "mean")
industry_avg_price <- industry_avg_price[order(industry_avg_price$Sector, industry_avg_price$Industry),]
colnames(industry_avg_price)[3] <- "Industry_Avg_Price"

# ISOLATE FINANCIAL SECTOR AND VISUALIZE IT
industry_chart <- subset(industry_avg_price, Sector == "Financial")

ggplot(industry_chart, aes(x = Industry, y = Industry_Avg_Price, fill = Industry)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Industry Average Price") + theme(legend.position = "none")

# ISOLATE Property & Casualty Insurance COMPANY AND VISUALIZE IT
company_chart <- subset(finviz, Industry == "Property & Casualty Insurance")

ggplot(company_chart, aes(x = Company, y = Price, fill = Company)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Company Average Price") + theme(legend.position = "none")

# REAVERAGE DATASET
finviz <- subset(finviz, Ticker != "BRK-A")
sector_avg_price <- aggregate(Price~Sector, data = finviz, FUN = "mean")
colnames(sector_avg_price)[2] <- "Sector_Avg_Price"

ggplot(sector_avg_price, aes(x = Sector, y = Sector_Avg_Price, fill = Sector)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Sector Average Price")

sector_avg_price <- aggregate(Price~Sector, data = finviz, FUN = "mean")

sector_avg_price <- ddply(finviz, "Sector", summarise, Price = mean(Price, na.rm = TRUE))

# FOR SECTORS
sector_avg <- melt(finviz, id = "Sector")

# CLEAN VARIABLES
sector_avg <- subset(sector_avg, variable %in% c("Price", "P.E", "PEG", "P.S", "P.B"))
sector_avg <- (na.omit(sector_avg))
sector_avg$value <- as.numeric(sector_avg$value)

sector_avg <- dcast(sector_avg, Sector~variable, mean)
colnames(sector_avg)[2:6] <- c("SAvPE", "SAvPEG", "SAvPS", "SAvPB", "SAvPrice")

# FOR INDUSTRY
industry_avg <- melt(finviz, id = c("Sector", "Industry"))

industry_avg <- subset(industry_avg, variable %in% c("Price", "P.E", "PEG", "P.S", "P.B"))
industry_avg <- (na.omit(industry_avg))
industry_avg$value <- as.numeric(industry_avg$value)

industry_avg <- dcast(industry_avg, Sector+Industry~variable, mean)
colnames(industry_avg)[3:7] <- c("IAvPE", "IAvPEG", "IAvPS", "IAvPB", "IAvPrice")

# MERGING ALL
finviz <- merge(finviz, sector_avg, by.x = "Sector", by.y = "Sector")
finviz <- merge(finviz, industry_avg, by.x = c("Sector", "Industry"), by.y = c("Sector", "Industry"))

# PLACEHOLDER
# TO TRACK WHETHER A STOCK IS UNDER VALUE
finviz$SPEUnder <- 0
finviz$SPEGUnder <- 0
finviz$SPSUnder <- 0
finviz$SPBUnder <- 0
finviz$SPriceUnder <- 0
finviz$IPEUnder <- 0
finviz$IPEGUnder <- 0
finviz$IPSUnder <- 0
finviz$IPBUnder <- 0
finviz$IPriceUnder <- 0

# SET TO ONE WHEREVER THE RESPECTIVE VALUE FOR THE STOCK IS LESS THAN THE AVERAGE
finviz$SPEUnder[finviz$P.E<finviz$SAvPE] <- 1
finviz$SPEGUnder[finviz$PEG<finviz$SAvPEG] <- 1
finviz$SPSUnder[finviz$P.S<finviz$SAvPS] <- 1
finviz$SPBUnder[finviz$P.B<finviz$SAvPB] <- 1
finviz$SPriceUnder[finviz$Price<finviz$SAvPrice] <- 1
finviz$IPEUnder[finviz$P.E<finviz$IAvPE] <- 1
finviz$IPEGUnder[finviz$PEG<finviz$IAvPEG] <- 1
finviz$IPSUnder[finviz$P.S<finviz$IAvPS] <- 1
finviz$IPBUnder[finviz$P.B<finviz$IAvPB] <- 1
finviz$IPriceUnder[finviz$Price<finviz$IAvPrice] <- 1

# CREATE NEW COL TO TELL IN A SCALE OF 10 HOW UNDERVALUED THE STOCK IS BASED 
finviz$RelValIndex <- apply(finviz[79:88],1,sum)

potentially_undervalued <- subset(finviz,RelValIndex>=8)

# WINDOWING (SCREENING) ANALYSIS (STOP HERE INTERNET NEEDED)
# Only US companies
# Price per share between $20 and $100
# Volume greater than 10,000
# Positive earnings per share currently and projected for the future
# Total debt to equity ratio less than 1
# Beta less than 1.5
# Institutional ownership less than 30 percent
# Relative valuation index value greater than 8
target_stock <- subset(finviz, Price > 20 & Price < 100 & Volume > 1000 & Country == "USA" & EPS..ttm. > 0 & EPS.growth.next.year > 0 & EPS.growth.next.5.years > 0 & Total.Debt.Equity < 1 & Beta < 1.5 & Institutional.Ownership<30 & RelValIndex > 8 )

counter <- 0

for(symbol in target_stock$Ticker){
  url <- paste0("http://ichart.finance.yahoo.com/table.csv?s=",symbol,"&a=08&b=7&c=1984&d=01&e=23&f=2014&g=d&ignore=.csv")
  
  stock <- read.csv(url)
  stock <- na.omit(stock)
  colnames(stock)[7] <- "AdjClose"
  stock[, 1] <- as.Date(stock[, 1])
  stock <- cbind(Symbol = symbol, stock)
}











