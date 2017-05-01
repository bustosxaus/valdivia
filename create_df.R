
# 4-21-2012 through 4-28-2017, magnitudes > 2.5
chile_5 = read.csv("datasets/chile_5.csv")
cali_5 = read.csv("datasets/cali_5.csv")

# magnitudes above 2.5 roughly around Chile
chile_df = chile_5 %>%
  filter(longitude < -60, longitude > -80, 
         latitude < -15, latitude > -60)

# magnitudes above 2.5 roughly around california to mexico
cali_df = cali_5 %>%
  filter(longitude > -125, longitude < -100, 
         latitude > 15, latitude < 42.5)

# 10 years of greater than 2.5 magnitude 

# 1 year of all magnitudes
bayarea = read.csv("datasets/bayarea3.csv") %>%
  filter(mag > 0)
bayarea$time = ymd_hms(bayarea$time)
bayarea_df = bayarea %>%
  filter(month(time) %in% 1:6)


write.csv(cali_df, "datasets/cali_df.csv")
write.csv(chile_df, "datasets/chile_df.csv")
