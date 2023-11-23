##### Homework 2: Due Wednesday 10/4/23, #################
#Neemias Moreira


# 1 Load the DataSetHurricanescsv file.  Filter for all of the storms that 
# affected only South Carolina (SC)

library(tidyverse)


hurricanes1 <- read.csv("DataSetHurricanes.csv")
summary(hurricanes1)

SC <- filter(hurricanes1, AffectedStates=="SC")

SC


#2 Find, using the filter and between functions, all of the storms with wind
# speeds in the range of 100 and 150 MPH.


STORM_SPEED <- filter(hurricanes1,  between(WindMPH, 100, 150))

str(STORM_SPEED)
summary(STORM_SPEED)



# 3 Create a tibble with all storms in 1995 sorted alphabetically. Use the 
# piping syntax.

library(tibble)

Storm1995 <-hurricanes1 %>%
  filter(Year==1995)
  
Storm1995


# 4 Create a tibble with all storms in 2008 and provide a count of the total 
# number of deaths caused by these storms.

Storms_2008 <- hurricanes1 %>%
  filter(Year==2008)%>%
  summarise(Total_Storms=n(), Total_Deaths = sum(Deaths))


Storms_2008


#5 Using the tibble created in Question 4, find the year with the maximum 
# number of deaths


Storm_Deaths <- hurricanes1 %>%
  group_by(Year)%>%
  summarise(Total_Storms=n(), Total_Deaths = sum(Deaths))


Storm_Deaths
str(Storm_Deaths)

max(Storm_Deaths$Total_Deaths)
summary(Storm_Deaths)

maxdeathsyear <- filter(Storm_Deaths, Total_Deaths == "1920")
maxdeathsyear

#2005 was the year of more deaths.



#############################################################################
#                                                                           #
#       For the remainder of the questions, use the ncyflights13 data set   #
#                                                                           #
#############################################################################

#6 Write the code that will allow one to plot the total number of delayed flights
# per month from LaGuardia (LGA) airport.

library(nycflights13)

LGA <-filter(flights, origin=="LGA")
LGA_DELAY<- filter(LGA, LGA$dep_delay>0)

LGA_DELAY

LGA_MONTH <- LGA_DELAY%>%
  group_by(month)%>%
  summarize(count=n())

LGA_MONTH

plot(LGA_MONTH$month, LGA_MONTH$count, type="b")
abline(h=mean(LGA_MONTH$count), col="black")


#7 Write the code that will allow one to plot the percentage of delayed flights
# per month from LaGuardia (LGA) airport.

LGA_Total <- LGA %>%
  group_by(month)%>%
  summarize(count=n())
LGA_Total 

LGA_MONTH <- mutate(LGA_MONTH, percent_delay=count/LGA_Total$count*100)
LGA_MONTH

plot(LGA_MONTH$month,LGA_MONTH$percent_delay, type="b")
abline(h=mean(LGA_MONTH$percent_delay), col="purple")


#8 Write the code to make a visualization, in a side by side fashion, of the 
# plots of percentage flight delayed for per month for each of the three airport.  
# What can you conclude about the delayed flights from these airports?

par(mfrow=c(1,3))
plot(EWR_MONTH$month,EWR_MONTH$percent_delay, type="b")
abline(h=mean(EWR_MONTH$percent_delay), col="black")
plot(JFK_MONTH$month,JFK_MONTH$percent_delay, type="b")
abline(h=mean(JFK_MONTH$percent_delay), col="red")
plot(LGA_MONTH$month,LGA_MONTH$percent_delay, type="b")
abline(h=mean(LGA_MONTH$percent_delay), col="purple")
par(mfrow=c(1,1))


# I can conclude LGA is the Airport with less percentage of delayed flights.
#JFK is the second with less percentage of delayed flights 
#EWR is the airport with more percentage of delayed flights.
#Overall between months 9 to 11 are the bests months to travel on those airports.




#9 Analyze the delayed flights per carrier per airport.  Write the code to produce 
# a visualization, in a side side fashion, of the percentage flights delayed vs 
# carrier.  Note that these plots will be barplots.  What can you conclude about 
# the delayed flights per carrier by airport?

LGA_CarrierDelay <- mutate(LGA_CarrierDelay, percent_delay=count/LGA_Total$count*100)

LGA_CarrierDelay

JFK_CarrierDelay <- mutate(JFK_CarrierDelay, percent_delay=count/JFK_Total$count*100)

JFK_CarrierDelay

EWR_CarrierDelay <- mutate(EWR_CarrierDelay, percent_delay=count/EWR_MONTH$count*100)

EWR_CarrierDelay

par(mfrow=c(1,3))

barplot(LGA_CarrierDelay$percent_delay, xlab = "Carrier", ylab = "Percentege delay")
barplot(JFK_CarrierDelay$percent_delay, xlab = "Carrier", ylab = "Percentege delay")
barplot(EWR_CarrierDelay$percent_delay, xlab = "Carrier", ylab = "Percentege delay") 

par(mfrow=c(1,1))




