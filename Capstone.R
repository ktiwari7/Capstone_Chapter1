#Loading Required Libraries
library(ggmap)
library(ggplot2)
library(maps)
library(rmaps)

#Reading the New York Accident data
setwd("C:/Users/Kshitij/Downloads")
a <- read.csv("New York Accident Data1.csv")
a

#To see which variables are in the data
names(a)

#To check any na values in the dataset
is.na(a)

# Combining the latitude and longitude variables of data set
a[5]
a[6]
c<-cbind(a[5],a[6])
c

# Checking and removing the na values from c
is.na(c)
table(is.na(c))
c[!complete.cases(c),]
d<-na.omit(c)
d

#New York Accident google map plot
setwd("C:/Users/Kshitij/Downloads")
a <- read.csv("New York Accident Data.csv")
names(a)
a$lat <- a$LATITUDE
a$lon <- a$LONGITUDE

map <- get_map(location='new york, ny', zoom=13, maptype='roadmap') 

ggmap(map) +
  geom_point(data = a, aes(x = lon, y = lat, fill = "blue", alpha = 0.5, size=(Day.Of.Week)), size = 1, shape = 20) +
  ggtitle("New York Accident Plot on Google Map") +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#density map plot of New York Accident Hotspots
ggmap(map) + stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = 0.5), 
  size = 3, bins = 8, data = a, geom = "polygon") +
  ggtitle("Density plot of NY Accidents") +
  scale_fill_gradient(low = "black", high = "blue")


#Performing Logistic Regression
View(a)
names(a)

logistic <- glm(Injured.or.Died ~ Total.People.Injured + Total.People.Died, data=a, 
                family = binomial(link = "logit")) 
summary(logistic)
coef(logistic)

#Graph plot of Main Contributing Factor for Accidents
plot(a$CONTRIBUTING.FACTOR.VEHICLE.1, a$Injured.or.Died, xlab = "Main Contributing Factor")

#Graph Plot of Total people died and total people Injured
plot(a$Total.People.Injured,a$Total.People.Died, xlab = "Total People Injured", ylab = "Total People Died")


#Reading the Boston Crash data
crash <- read.csv("Crash.csv")

#Reading the New York data which contains variables similar to the Boston data
a <- read.csv("ny.csv")
View(crash)

#Combining both the data sets of New York and Boston
full <- rbind(crash, a)
names(full)
full

# writing the file full
write.csv(file = "full.csv", x = full)
View(full)

library(ggmap)

#Defining from and to and using it in route function
from <- c("Saint Peters University, Jersey City , USA")
to <- c("Liberty Mutual Tower, Boston, USA")
mapdist(from, to)
route_df <- route(
  from,
  to,
  structure = 'route',
  mode = 'driving',+alternatives = TRUE
)

#Using the route_df defined above to plot alternative routes in R
qmap('New York, USA', zoom = 8) +
  geom_path(
    aes(x = lon, y = lat),
    colour = 'blue',
    size = 1.6,
    data = route_df,
    lineend = 'round'
  )

