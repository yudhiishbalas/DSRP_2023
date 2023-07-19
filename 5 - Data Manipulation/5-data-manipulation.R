library(dplyr)
library(ggplot2)

data_Set <- read.csv("data/Numeric.csv")
#head(data_Set)
#View(data_Set)

AWDCars <- filter(data_Set, Drive == "All Wheel Drive")
#View(AWDCars)
Mercs <- filter(data_Set, Make == "Mercedes")
#View(Mercs)

mainEVstats <- select(data_Set, -BatterySizekWH, -EfficiencyWHPKM, -FastChargeSpeedKMPH)
#View(mainEVstats)

timeISmoney <- mutate(data_Set,
                   PriceinUSADollar = round(ifelse(is.na(PriceinGermanyEuro),PriceinUKPounds*1.30,PriceinGermanyEuro*1.12), digits=0),
                   WLTPChargingSpeedMIN = round((RangeKM/FastChargeSpeedKMPH)*60, digits=0),
                   RangeMI = round(RangeKM*0.621371, digits=0))
#View(timeISmoney)
data_Set <- timeISmoney

avgUSprice <- summarize(data_Set,meanUSAPrice = mean(PriceinUSADollar, na.rm = T))
#View(avgUSprice)

highestRange <- arrange(data_Set, desc(RangeKM), AccelerationSEC,desc(TopSpeedKMPH))
#View(highestRange)
data_Set <- highestRange



# Plot 1 Distribution
ggplot(data = data_Set, aes(x = PriceinUSADollar)) + 
  geom_histogram() +
  labs(x = "US Prices ($)",
       y = "Number of Electric Vehicles",
       title = "EV Price Distribution in the US ($)")

#Plot 2 Numeric vs Categorical
ggplot(data = data_Set, aes(x = Drive,y = RangeMI)) + 
  geom_violin(aes(fill=Drive)) +
  labs(x = "EV Drivetrain",
       y = "Drivable Range (miles)",
       title = "EV Drivetrain vs Drivable Range (miles)")

#Plot 3 Numeric vs Numeric
ggplot(data = data_Set, aes(x = BatterySizekWH, y = RangeMI)) + 
  geom_point(aes(color=Drive)) +
  labs(x = "Battery Size (kiloWatt Hours)",
       y = "Drivable Range (miles)",
       title = "Battery Size (kiloWatt Hours) vs Drivable Range (miles)") + 
  scale_color_manual(values=c("blue","red","yellow"))+
  geom_smooth(color="gray1")

