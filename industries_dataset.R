#folder with industry/acquired/bankruptcy data
ind = read.csv("stockpupindustries.csv",header = TRUE, stringsAsFactors = F)

#split industry into industries and gather with tidy
ind[ind$industry =='nan',]
ind[ind$ticker=='SWY','industry'] = "consumer goods, retail"
ind[ind$ticker=='IQV','industry'] = "pharmaceuticals, medical devices"
ind[ind$ticker=='AGL','industry'] = "energy"  
ind[ind$ticker=='BLK','industry'] = "asset management, financial company"  
ind[ind$ticker=='ATGE','industry'] = "consumer services"

ind <- ind %>% select(-index)

write.csv(ind, file = 'industries.csv', row.names = F)

###############

industries <- cSplit(ind, "industry", ",")
industries <- industries %>% gather(key = 'industry_n', value = 'industry', industry_1:industry_6, na.rm = T)
unique(industries$industry)

write.csv(industries, file = 'tidy_industries.csv', row.names = F)

colnames(industries)
head(industries)
#################INDUSTRIES DATASET CREATED##################