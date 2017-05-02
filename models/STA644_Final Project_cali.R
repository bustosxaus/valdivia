############ STA 644 FINAL PROJECT ##############

library(data.table)
library(ggplot2)
library(raster)
library(magrittr)
library(modelr)
library(tidyr)
library(stringr)
library(gridExtra)
library(purrr)
library(forecast)
library(fields)
library(sf)
library(forcats)
library(dplyr)
library(lubridate)
library(geoR)
library(spBayes)
library(maptools)

# Reading in the earthquakes data for the California
cali = fread("bayarea.csv")

# Colin's functions
get_coda_parameter = function(coda, pattern)
{
  w = coda[[1]] %>% colnames() %>% str_detect(pattern)
  coda[[1]][,w,drop=FALSE]
}

post_summary = function(m, ci_width=0.95)
{
  d = data_frame(
    post_mean  = apply(m, 2, mean),
    post_med   = apply(m, 2, median),
    post_lower = apply(m, 2, quantile, probs=(1-ci_width)/2),
    post_upper = apply(m, 2, quantile, probs=1 - (1-ci_width)/2)
  )
  
  if (!is.null(colnames(m)))
    d = d %>% 
      mutate(param = colnames(m)) %>% 
      dplyr::select(param, post_mean:post_upper)
  
  d
}

strip_attrs = function(obj)
{
  attributes(obj) = NULL
  obj
}

strip_class = function(obj)
{
  attr(obj,"class") = NULL
  obj
}

morans_I = function(y, w)
{
  n = length(y)
  y_bar = mean(y)
  num = sum(w * (y-y_bar) %*% t(y-y_bar))
  denom = sum( (y-y_bar)^2 )
  (n/sum(w)) * (num/denom)
}


# Getting map of California
cali_map = map_data("state")
cali_map = cali_map %>%
  filter(region == "california")
# Map of Mexico
world_map = map_data("world")
mexico_map = world_map %>%
  filter(region == "Mexico")
# Merging the two
both_map = bind_rows(cali_map, mexico_map)

# Plotting magnitude
ggplot() +
  geom_map(data = both_map, map = both_map, aes(map_id = region),
           color = "black", fill = "white") +
  geom_point(data = cali, aes(x = longitude, y = latitude, color = mag), alpha = .5) +
  scale_color_continuous(low = "grey", high = "red") +
  scale_size(range = c(2, 6)) +
  coord_fixed(1.3)


# Getting the coordinates of our data
coords = cali[, .(longitude, latitude)] %>% 
  as.matrix()

# Distance matrix
w = dist(coords) %>% 
  as.matrix()

# Calculating Moran's I
moransI = morans_I(y = cali$mag, w = w)

############### VARIOGRAM ################

# Sample of the data for testing
cali_samp = cali

# Getting the coordinates of our data
coords = cali_samp[, .(longitude, latitude)] %>% 
  as.matrix()

# Calculating the distances between points
d = dist(coords) %>%
  as.matrix()

# Calculating and plotting the variogram
variog(coords = coords, data = cali_samp$mag, messages = FALSE,
        uvec = seq(0, max(d)/2, length.out = 50)) %>%
   plot()

# Number of observations in the data
n = cali_samp[, .N]
# Number of MCMC samples
n_samp = 20000
# Max range 
max_range = max(d) / 4

# Formula for models
form = formula(mag ~ depth)

# Model matrix for our model
model_matrix = model.matrix(data = cali_samp, form)

# Number of covariates
p = ncol(model_matrix)

# Beta prior
beta_cov = diag(1000, p)

# Starting values for our Bayesian sampler
starting = list(phi = .1, sigma.sq = .03, tau.sq = .1)
# Tuning values
tuning = list("phi" = 0.03, "sigma.sq" = 0.03, "tau.sq" = 0.03)
# Priors for our Bayesian Sampler
priors = list(
  beta.Norm = list(rep(0, p), beta_cov),
  phi.Unif = c(.01, 3),
  sigma.sq.IG = c(2, 2),
  tau.sq.IG = c(2, 2)
)

# Fitting the model
m = spLM(form, data = cali_samp, coords = coords, 
         starting = starting, priors = priors,
         cov.model = "exponential", n.samples = n_samp, tuning = tuning,
         n.report = n_samp / 2)

# Recovering samples
samples = spRecover(m, start = n_samp/2 + 1)

# Posterior analysis
samples$p.theta.samples %>%
  mcmc() %>%
  plot()

# Posterior distribution of beta
samples$p.beta.recover.samples %>%
  mcmc() %>%
  plot()

# Posterior summaries
samples$p.beta.recover.samples %>%
  post_summary() %>%
  knitr::kable(digits=5)

# Reading in the world map spatial dataset
data(wrld_simpl)
# Creating a raster for the region we care about
r = raster(nrows = 200, ncol = 200,
           xmn = min(cali_samp$longitude) * 1.05, xmx = max(cali_samp$longitude) * 0.95,
           ymn = min(cali_samp$latitude) * 0.95, ymx = max(cali_samp$latitude) * 1.05)

# Rasterizing the cali portion of the world map
cali_raster = rasterize(wrld_simpl[wrld_simpl$NAME %in% c("United States", "Mexico"), ], r)
# r_raster = rasterize(r)

# Stripping empty portions?
cells = which(is.na(cali_raster[]) == FALSE)
# Getting the cooridinates from the raster
pred_coords = xyFromCell(r, cells)


# Predicting on the raster
pred = spPredict(samples, pred_coords,
                 pred.covars = matrix(1, nrow = nrow(pred_coords), ncol = ncol(beta_cov)),
                 start = n_samp - 100)
# Posterior summary of the predictions
pred_summary = post_summary(t(pred$p.y.predictive.samples))

# The raster
splm_pred = r
splm_pred[cells] = pred_summary$post_mean
# splm_pred[1:nrow(pred_coords)] = pred_summary$post_mean

plot(splm_pred, xlim = c(-122.5, -121.25), ylim = c(36.8, 38))
points(coords, pch=16, cex=0.5)

# Getting the fitted values for our data points
pred_data = spPredict(samples, coords,
                      pred.covars = matrix(c(rep(1, nrow(coords)), 
                                             cali_samp$depth), 
                                           nrow = nrow(coords)),
                      start = n_samp - 1000)


# Getting the residuals from our model
resid = rowMeans(pred_data$p.y.predictive.samples) - cali_samp$mag
# RMSE
RMSE = sqrt(mean(resid^2))
RMSE
