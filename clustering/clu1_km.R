library(dplyr)
library(sqldf)


data <- read.csv('rainfall_history_alert.csv', header = TRUE)



####Check Data 
sapply(data,function(x) sum(is.infinite(x))/len) #Check Inf
sapply(data,function(x) sum(is.na(x))/len) #Check na


#### Length of data
len <- nrow(data)

####Replace -Inf as 0
data2 <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))


####Extract data without slope
dat_clu1 <- data2[,c(7:31)]

####Kmeans
clu1_km = kmeans(dat_clu1, centers = 10, nstart = 25, iter.max = 20)
fviz_cluster(clu1_km, dat_clu1, frame = FALSE, geom = "point")

clu1_km_op <- data.frame(clu1_km$cluster,data)
saveRDS(object = clu1_km_op, file = "clu1_km_op.rds")





clu1_km_op2=do.call(data.frame,lapply(clu1_km_op, function(x) replace(x, is.infinite(x),0)))



clu1_km_op2 %>%
  arrange(desc(pcpn_1h_max),desc(pcpn_24h_max))%>%
  rename(clu1_km_cluster=clu1_km.cluster)->fnl

fnl %>% 
  group_by(clu1_km_cluster) %>%
  summarise(avg_24_amount = mean(pcpn_24h_max),avg_1_amount = mean(pcpn_1h_max)) %>%
  arrange(desc(avg_1_amount),desc(avg_24_amount)) 


final <- sqldf("select case when clu1_km_cluster = 2 then 10
                          when clu1_km_cluster = 4 then 9
                          when clu1_km_cluster = 3 then 8
                          when clu1_km_cluster = 7 then 7
                          when clu1_km_cluster = 6 then 6
                          when clu1_km_cluster = 5 then 5
                          when clu1_km_cluster = 9 then 4
                          when clu1_km_cluster = 10 then 3
                          when clu1_km_cluster = 8 then 2
                          when clu1_km_cluster = 1 then 1
                     end as clu1_km_act
                     ,*
              from fnl")

saveRDS(object = final, file = "C:\\Users\\Milalaliu\\Desktop\\clu1_km_op_actl.rds")