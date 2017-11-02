##  STEP ONE
# INSTALL PACKAGES
install.packages("plyr")
install.packages("ggplot2")
install.packages("reshape2")

# LOAD PACKAGES, DATA, AND CHECKING IT
library(plyr)
library(ggplot2)
library(reshape2)

# SET SESSION PATH
setwd("D:/Master/Projects/DataScience/R/Automobile")

# READ AND VIEW DATA

# OPTION ONE
vehicles <- read.csv(unz("vehicles.csv.zip", "vehicles.csv"), stringsAsFactors = FALSE)
# OPTION TWO
vehicles <- read.csv("vehicles.csv", stringsAsFactors = FALSE)

head(vehicles)

# READ AND VIEW VARIABLE LABELS
labels <- do.call(rbind, strsplit(readLines("varlabels.csv"), " - "))
head(labels)

##  STEP TWO
# EXPLORE DATA
# DIMENSIONS OF DATA
nrow(vehicles)  # 34287 ROWS
ncol(vehicles)  # 74    COLS

names(vehicles)

length(unique(vehicles[, "year"]))

first_year <- min(vehicles[, "year"]) # GETTING FIRST YEAR
last_year <- max(vehicles[, "year"])  # GETTING LAST YEAR


# MOST CARS USES REGULAR GASOLINE AFTER IT PREMIUM GASOLINE
table(vehicles$fuelType1)

count(vehicles$trany == "")
unique(vehicles$trany)
vehicles$trany[vehicles$trany == ""] <- NA

# FEATURE ENG NEW COL FROM TRANY W/ FACTORS [MANUAL/ AUTO]
# taking first 4 char if they equals to "Auto" then Auto else set it to Manual
vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) == "Auto", "Auto", "Manual")

# transform trant to factors
vehicles$trany <- as.factor(vehicles$trany)

table(vehicles$trany2)

# SEE MODELS BY YEAR WITH SUPERCHARGER AND WITHOUT
with(vehicles, table(sCharger, year))

# DATA LOOK WEIRD
# HERE CHECK IF VEHICLE HAVE A SUPER CHARGE (S)
class(vehicles$sCharger) # "S", ""
unique(vehicles$sCharger)

# HERE CHECK IF VEHICLE HAVE A TURBO (T)
class(vehicles$tCharger) # "T", ""
unique(vehicles$tCharger)

# SO T IS NOT STAND FOR TRUE BUT R IS MISUNDERSTOOD PARSING

##  STEP THREE
#Analyzing automobile fuel efficiency over time

# take the vehicles data frame, aggregate rows by year, and then, for each group, we compute the mean highway, city, and combine fuel efficiency. The result is then assigned to a new data frame, mpgByYr
mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG = mean(comb08), avgHghy = mean(highway08), avgCity = mean(city08))

# we might conclude that there has been a tremendous increase in the fuel economy of cars sold in the last few years
#  However, this can be a little misleading as there have been more hybrid and non-gasoline vehicles in the later years
ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("All cars")


# CREATING NEW SUBSET OF DATAFRAME FOR CARS THAT USES GAS ONLY.
gasCars <- subset(vehicles, fuelType1 %in% c("Regular Gasoline", "Premium Gasoline", "Midgrade Gasoline") & fuelType2 == "" & atvType != "Hybrid")

# GETTING AND PLOTTING CARS THAT USES ONLY GAS
mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08))
ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Gasoline cars")

# THE TYPE MUST BE NUMERIC NOT CHAR
typeof(gasCars$displ)                       # CHARACTER

gasCars$displ <- as.numeric(gasCars$displ)  # DOUBLE

#This scatter plot of the data offers the convincing evidence that there is a negative, or even inverse correlation, between engine displacement and fuel efficiency; thus, smaller cars tend to be more fuel-efficient.
ggplot(gasCars, aes(displ, comb08)) + geom_point() + geom_smooth()

# Now, let's see whether more small cars were made in later years, which can explain the drastic increase in fuel efficiency
avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl = mean(displ))
ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average engine displacement (l)")

# Combining avg. MPG cars and engine displacement
byYear <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08), avgDispl = mean(displ))
head(byYear)

# Melt data for nice plotting(from cols to rows)
byYear2 = melt(byYear, id = "year")
levels(byYear2$variable) <- c("Average MPG", "Avg engine displacement")
head(byYear2)

# COMPARING PLOT BETWEEN AVG. MPG AND AVG. ENGINE DISPLACEMENT (FACETS)
ggplot(byYear2, aes(year, value)) + geom_point() + geom_smooth() + facet_wrap(~variable, ncol = 1, scales = "free_y") + xlab("Year") + ylab("")

# ISOLATE CARS WITH ONLY 3 CYLINDER FOR MORE STYDING
gascars4 <- subset(gasCars, cylinders == "4")

# COMPARING PLOT(BOX) BETWEEN AVG. MPG, YEAR, COMB08(AUTO, MANUAL)(FACETS)
ggplot(gascars4, aes(factor(year), comb08)) + geom_boxplot() + facet_wrap(~trany2, ncol = 1) + theme(axis.text.x = element_text(angle = 45)) + xlab("Year") + ylab("MPG")

# COMPARING PLOT(STACK BARS) BETWEEN PROPOTIONS OF CARS, YEAR, COM08(AUTO, MANUAL)
ggplot(gascars4, aes(factor(year), fill = factor(trany2))) + geom_bar(position = "fill") + geom_hline(yintercept = 0.5, linetype = 2) + theme(axis.text.x = element_text(angle = 45)) + labs(x = "Year", y = "Propotion of Cars", fill = "Transmission")

# GETTING CARS MAKERS WHO MAKE 4 CYLINDER CARS TO STUDY THE MOST COMPANY MODELS HAVE AVG. MPG
carsMake <- ddply(gascars4, ~year, summarise, numberOfMakes = length(unique(make)))

# PLOTTING CAR MAKERS NUMBER OF MODELS PER YEAR
ggplot(carsMake, aes(year, numberOfMakes)) + geom_smooth() + geom_point() + ylab("Number of Available Makes") + ggtitle("Four Cylinder Cars")

# GETTING CARS MAKERS NAMES PER YEAR (LIST)
uniqueMakes <- dlply(gascars4, ~year, function(x) unique(x$make))
# GETTING A LIST OF CARS MAKER COMPANY NAMES
commonMakes <- Reduce(intersect, uniqueMakes)
commonMakes

# GETTING ALL DATA WITH THE SAME CAR MAKERS COMPANY NAMES AND PLOT COMPARINT BETWEEN THEM
carsCommonMakes <- subset(gascars4, make %in% commonMakes)
avgMPG_commonMakes <- ddply(carsCommonMakes, ~year + make, summarise, avgMPG = mean(comb08))

ggplot(avgMPG_commonMakes, aes(year, avgMPG)) + geom_line() + facet_wrap(~make, nrow = 3)
