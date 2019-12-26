# MALNUTRITION IN GHANA --------------------------------------------------------

# Load malnutrition data for Ghana 
malnutrition <- readr::read_csv("data/malnutrition.csv")

# Load Ghana boundaries
library(sf)
ghana <- st_read("data/geodata/gadm36_GHA.gpkg", layer = "gadm36_GHA_0")

# Calculate the average HAZ for each location, transform it to a spatial object
# with sf class and plot it on the map
library(dplyr)
maln_sp <- malnutrition %>% 
  group_by(lng, lat) %>% 
  summarise(HAZ = mean(HAZ)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

library(tmap)
tm_shape(ghana) +
  tm_borders("black", lwd = 1) +
tm_shape(maln_sp) +
  tm_symbols(col = "HAZ", size = .04, shape = 19, palette = "RdYlBu", 
             contrast = c(0, 1), style = "cont", breaks = seq(-3, 3, by = 1)) +
tm_compass(position = c("left", "bottom")) +
tm_scale_bar(position = c("right", "bottom")) +
tm_layout(legend.frame = "black",  
          legend.position = c("left", "top"), 
          outer.margins = 0, asp = 0) 

# Interactive map  
library(mapview)
mapview(maln_sp)

# Explore relationship with covariates
library(ggplot2)

# HAZ vs Age
ggplot(malnutrition, aes(x = age, y = HAZ)) +
  geom_point(alpha = .3) +
  geom_smooth() +
  labs(x = "Age") 

# HAZ vs maternal education
ggplot(malnutrition, aes(x = factor(edu), y = HAZ)) +
  geom_boxplot() +
  labs(x = "Maternal education")

# HAZ vs Wealth index
ggplot(malnutrition, aes(x = factor(wealth), y = HAZ)) +
  geom_boxplot() +
  labs(x = "Wealth index")


# Linear model
lm_fit <- lm(HAZ ~ poly(age, 2) + edu + wealth, data = malnutrition)
summary(lm_fit)

beta.hat <- coef(lm.fit)
age.set <- seq(0,5,length=1000)
age.vars <- cbind(age.set,max.vec(1,age.set),max.vec(2,age.set))
broken.sticks <- as.numeric(age.vars%*%beta.hat[2:4])
std.errors <- sqrt(diag(age.vars%*%vcov(lm.fit)[2:4,2:4]%*%t(age.vars)))
ci.95 <- cbind(broken.sticks-qnorm(0.975)*std.errors,
               broken.sticks+qnorm(0.975)*std.errors)

matplot(age.set,cbind(ci.95,broken.sticks),type="l",xlab="Age (years)",
        ylab="",
        lty=c("dashed","dashed","solid"),col = 1)

