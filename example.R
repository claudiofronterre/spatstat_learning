# Load REMO data
oncho <- readr::read_csv("data/LiberiaRemoData.csv")

# Load Liberia boundaries
library(sf)
liberia <- st_read("data/geodata/gadm36_LBR.gpkg", layer = "gadm36_LBR_0")
plot(liberia$geom)

# Convert both oncho data and the map to UTM (EPSG:32629)
crs_utm_km <- "+init=epsg:32629 +units=km"
oncho_sp <- oncho %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = crs_utm_km)

liberia <- st_transform(liberia, crs = crs_utm_km)

# Visualise the data on a map
library(mapview)
oncho_sp$prevalence <- (oncho_sp$npos / oncho_sp$ntest) * 100
mapview(oncho_sp, zcol = "prevalence")


# Fit a binomial model
oncho[, c("utm_x", "utm_y")] <- st_coordinates(oncho_sp)
glm_fit <- glm(cbind(npos, ntest - npos) ~ utm_x + utm_y,
               data = oncho, family = binomial)

summary(glm_fit)

# # Generate grid for prediction
# grid_pred <- st_make_grid(liberia, cellsize = 10, what = "centers")
# plot(grid_pred)
# pred_coord <- st_intersection(grid_pred, liberia)
# 
# # Estiamtes of the regression coefficients
# beta.hat <- coef(glm_fit)
# 
# # Matrix of the explanatory variables at prediction locations
# D.pred <- as.matrix(cbind(1, st_coordinates(pred_coord)))
# 
# # Linear predictor at the prediction locations
# eta.hat <- D.pred%*%beta.hat
# 
# # Covariance matrix of the regression coefficients
# beta.covar <- vcov(glm.fit)
# 
# # Standard errors of the linear predictor
# se.eta.hat <- sqrt(diag(D.pred%*%beta.covar%*%t(D.pred)))
# 
# # Exceedance probabilities of 20% threshold
# exceed.20 <- 1 - pnorm(-log(4), mean = eta.hat, sd = se.eta.hat)
# 
# # Plot of the exceedance probabilities
# library(raster)
# predictions <- rasterFromXYZ(cbind(st_coordinates(pred_coord), exceed.20))
# crs(predictions) <- st_crs(liberia)$proj4string
# mapview(predictions)
# lines(Liberia.bndrs/1000,type="l")     

# Test spatial correlation
library(PrevMap)
check.spat <- spat.corr.diagnostic(npos ~ utm_x + utm_y,
                                   units.m = ~ ntest, 
                                   coords = ~ utm_x + utm_y,
                                   data = as.data.frame(oncho),
                                   likelihood = "Binomial",
                                   uvec = seq(20, 300, length = 15),
                                   n.sim = 1000)