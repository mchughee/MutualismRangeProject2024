### Read in packages
library(dplyr)
library(tidyr)

# Make summary dataframe

data<-read.csv("thinned_data_climate.csv")

# summarize data

data1<-data %>% drop_na(precip) %>% drop_na(temp)

summary_df<-data1 %>% 
  group_by(species) %>% 
  reframe(maxquant_precip=quantile(precip, 0.95), 
          minquant_precip=quantile(precip, 0.05),
          mean_precip=mean(precip),
          median_precip=median(precip),
          maxquant_temp=quantile(temp, 0.95),
          minquant_temp=quantile(temp, 0.05),
          mean_temp=mean(temp),
          median_temp=median(temp),
          max_lat=max(Y),
          min_lat=min(Y),
          mean_lat=mean(Y),
          median_lat=median(Y),
          quant95=quantile(Y, 0.95),
          quant005=quantile(Y, 0.05)
          )


# write into csv file
write.csv(summary_df, "summary_df_august2024.csv")
