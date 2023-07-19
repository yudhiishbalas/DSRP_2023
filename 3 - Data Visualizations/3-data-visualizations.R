library(dplyr)
data_Set <- read.csv("data/Numeric.csv")

# Plot 1 Distribution
ggplot(data = data_Set, aes(x = BatterySizekWH)) + 
  geom_histogram() +
  labs(x = "Battery Size (kiloWatt Hours)",
       y = "Number of Electric Vehicles",
       title = "Battery Size Distribution (kiloWatt Hours)")

#Plot 2 Numeric vs Categorical
ggplot(data = data_Set, aes(x = Drive,y = EfficiencyWHPKM)) + 
  geom_violin(aes(fill=Drive)) +
  labs(x = "EV Drivetrain",
       y = "Efficiency (WattHours per km)",
       title = "EV Drivetrain vs Efficiency (WattHours per km)")

#Plot 3 Numeric vs Numeric
ggplot(data = data_Set, aes(x = TopSpeedKMPH, y = AccelerationSEC)) + 
  geom_point(aes(color=Drive)) +
  labs(x = "Top Speed (km/h)",
       y = "Acceleration (seconds)",
       title = "Top Speed (km/h) vs Acceleration (seconds)") + 
  scale_color_manual(values=c("blue","red","yellow"))+
  geom_smooth(color="gray1")
