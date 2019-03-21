library(ggmap)
library(stringr)
library(pglm)
register_google(key = "AIzaSyBCQN8_QIAvVpeT6zeslqVR6Y71nR7V94U", account_type = "standard")
#Importing Data
df1 <- Code_01A_timedat[,c(2:4)]
df1 <- df1[complete.cases(df1),]

#Extracting Zip Codes
#daily_limit <- 2500
out_mat <- matrix(data = 0, nrow = nrow(df1), ncol = 1)

for(j in 1:nrow(out_mat)){
  
  cc <- as.numeric(df1[j,c(2:3)])
  out <- revgeocode(rev(cc), output = "address", force = TRUE)
  x <- unlist(strsplit(out, "IL"))
  x <- unlist(strsplit(x[2], "USA"))
  x <- unlist(strsplit(x, ","))
  
  out_mat[j,1] <- as.numeric(x[1]) 
}

#Collecting NAs
na_id <- which(is.na(out_mat))

time_dat <- df1[-c(na_id),1]
out_mat <- out_mat[-c(na_id),1]


#Date extraction
tda <- as.character(unlist(time_dat))

for (j in 1:length(tda)) {
  
  x <- strsplit(tda[j], " ")
  y <- as.Date(x[[1]][1], format = "%m/%d/%Y")
  y <- format(y, "%Y-%m")
  
  tda[j] <- y
  
}

#Plotting Distribution
barplot(table(out_mat), beside=TRUE, legend = FALSE)
#Collecting zipcode with highest crime occurences
#high_zip_val <- as.numeric(unlist(names(which(table(out_mat) == max(table(out_mat))))))
#occur_crime <- which(out_mat == high_zip_val)
#time_occur_crime <- tda[occur_crime]

idx <- format(seq(ISOdate(2010,11,1), by = "month", length.out = 99), "%Y-%m")

#Creating Independent Variable
z1 <- as.numeric(unlist(z1))
z2 <- as.numeric(unlist(z2))
z3 <- as.numeric(unlist(z3))
z4 <- as.numeric(unlist(z4))

#Creating Dataframe
df2 <- matrix(data = 0, nrow = length(oy), ncol = 5)
df2[,1] <- oy
df2[,2] <- z1
df2[,3] <- z2
df2[,4] <- z3
df2[,5] <- z4

#Creating response variable
ymat <- matrix(data = 0, nrow = length(tda), ncol = 2)
for (j in 1:nrow(ymat)) {
  
  ymat[j,1] <- tda[j]
  ymat[j,2] <- out_mat[j]
  
  
}

#Subsetting Data as per starting date of Zillow Data
s_id <- which(ymat[,1] == idx[1])[1]
ep_id <- length(idx)
e_id <- which(ymat[,1] == idx[ep_id])
e_id <- e_id[length(e_id)]
#Response Variables along with fixed effect - ZIP
rymat <- ymat[c(s_id:e_id),]
rxmat <- matrix(data = 0, nrow = nrow(rymat), ncol = 12)
z1 <- BuyerSellerIndexCrossGeo_Zip[which(BuyerSellerIndexCrossGeo_Zip[,4]=="IL"),]
z1 <- z1[,c(2,8:106)]
z2 <- BuyerSellerIndexCrossTime_Zip[which(BuyerSellerIndexCrossTime_Zip[,4]=="IL"),]
z2 <- z2[,c(2,8:106)]
z3 <- Zip_Zri_AllHomes[which(Zip_Zri_AllHomes[,3]=="Chicago"),]
z3 <- z3[,c(2,8:106)]
z4 <- Sale_Counts_Zip[which(Sale_Counts_Zip[,3] == "Illinois"),]
z4 <- z4[,c(2,37:135)]
z5 <- Zip_MedianListingPrice_AllHomes[which(Zip_MedianListingPrice_AllHomes[,2] == "Chicago"),]
z5 <- z5[,c(1,17:115)]
z6 <- Zip_MedianPctOfPriceReduction_AllHomes[which(Zip_MedianPctOfPriceReduction_AllHomes[,3]=="Chicago"),]
z6 <- z6[,c(2,10:108)]
z7 <- Zip_MedianRentalPrice_AllHomes[which(Zip_MedianRentalPrice_AllHomes[,2] == "Chicago"),]
z7 <- z7[,c(1,16:114)]
z8 <- Zip_MedianRentalPricePerSqft_AllHomes[which(Zip_MedianRentalPricePerSqft_AllHomes[,2] == "Chicago"),]
z8 <- z8[,c(1,16:114)]
z9 <- Zip_PctOfHomesDecreasingInValues_AllHomes[which(Zip_PctOfHomesDecreasingInValues_AllHomes[,3] == "Chicago"),]
z9 <- z9[,c(2,172:270)]
z10 <- Zip_PctOfHomesIncreasingInValues_AllHomes[which(Zip_PctOfHomesIncreasingInValues_AllHomes[,3] == "Chicago"),]
z10 <- z10[,c(2,172:270)]
z11 <- Zip_PctOfListingsWithPriceReductions_AllHomes[which(Zip_PctOfListingsWithPriceReductions_AllHomes[,3] == "Chicago"),]
z11 <- z11[,c(2,10:108)]
z12 <- Zip_PriceToRentRatio_AllHomes[which(Zip_PriceToRentRatio_AllHomes[,3] == "Chicago"),]
z12 <- z12[,c(2,9:107)]


for (k in 1:nrow(rymat)) {#First look at the Zip code & year
  
  tuple_zip_year <- rymat[k,] #one-to-One correspondence between rx and rymat
  
  if(length(which(z1[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,1] <- as.numeric(unlist(z1[which(z1[,1] == tuple_zip_year[2]),pid+1]))
  }
  
  
  if(length(which(z2[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,2] <- as.numeric(unlist(z2[which(z2[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  
  if(length(which(z3[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,3] <- as.numeric(unlist(z3[which(z3[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  
  if(length(which(z4[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,4] <- as.numeric(unlist(z4[which(z4[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z5[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,5] <- as.numeric(unlist(z5[which(z5[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z6[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,6] <- as.numeric(unlist(z6[which(z6[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z7[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,7] <- as.numeric(unlist(z7[which(z7[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z8[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,8] <- as.numeric(unlist(z8[which(z8[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z9[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,9] <- as.numeric(unlist(z9[which(z9[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z10[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,10] <- as.numeric(unlist(z10[which(z10[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z11[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,11] <- as.numeric(unlist(z11[which(z11[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  if(length(which(z12[,1] == tuple_zip_year[2]))>0){#First check if ZIP exists in Zillow Dataset
    pid <- which(idx == tuple_zip_year[1])
    rxmat[k,12] <- as.numeric(unlist(z12[which(z12[,1] == tuple_zip_year[2]),pid+1]))
    
  }
  
  
}

#Creating Datamatrix
df2 <- cbind(rymat,rxmat)
df2 <- df2[complete.cases(df2),]
df2 <- as.data.frame(df2)
colnames(df2) <- c("month", "zip", "bsi_geo", "bsi_time", "zri", "sale_count", "med_list_price", "med_pct_price_red", "med_rent_price", "med_rent_price_sqft", "pct_home_decre_val", "pct_home_incre_val", "pct_list_price_red", "price_to_rent_ratio")

library(plm)

reg2 <- lm(sale_count ~ bsi_geo+bsi_time+zri, data = df3)


library(tidyverse)
df2 %>%
  group_by(zip) %>%
  expand(month) %>% #in each id expand the dates
  left_join(df2) -> df3 #join the original data frame and save to object df1

df3 <- as.data.frame(df3)

df5 <- df3[,c(3:14)]


for (i in 1:ncol(df5)) {
  
  df5[,i] <- as.numeric(unlist(df5[,i]))
  
}

df6 <- data.frame("month" = df3[,2], "zip" = df3[,1], "bsi_geo" = df5[,1], "bsi_time" = df5[,2], "zri" = df5[,3], "sale_count" = df5[,4], "med_list_price" = df5[,5], "med_pct_price_red" = df5[,6], "med_rent_price" = df5[,7], "med_rent_price_sqft" = df5[,8], "pct_home_decre_val"= df5[,9], "pct_home_incre_val"= df5[,10], "pct_list_price_red"= df5[,11], "price_to_rent_ratio"= df5[,12])

#Removing repeated entries from dataset
mtlist <- list()
for (i in 1:length(unique(df6[,2]))) {
  
  a <- df6[which(df6[,2] == unique(df6[,2])[i]),]
  a <- a[!duplicated(a[,1]),]
  
  mtlist[[i]] <- a
  
}

#Now creating dataframe
mtdf <- do.call(rbind.data.frame, mtlist)

#Counting Crime occurences for each Zip and month
clist <- list()
#crim_mat[,1] <- idx
for (j in 1:length(unique(rymat[,2]))) {#For each zip code count
  crim_mat <- matrix(data = 0, nrow = length(idx), ncol = 2)
  zz <- which(rymat[,2] == unique(rymat[,2])[j])
  az <- rymat[zz,1] #Capturing dates on which the Zip code reported murder
  
  for (k in 1:length(az)) {
    
    ia <- which(idx == az[k])
    crim_mat[ia,2] <- crim_mat[ia,2]+1
  }
  
  clist[[j]] <- crim_mat
  
}

cmdf <- do.call(rbind.data.frame, clist)

mtdf2 <- cbind(mtdf, occ_crim)

#Panel dATA FE Regression
ft2 <- plm(occ_crim ~ bsi_geo + zri + sale_count + med_list_price + med_pct_price_red + med_rent_price + med_rent_price_sqft + pct_home_decre_val + pct_home_incre_val + pct_list_price_red + price_to_rent_ratio, data = mtdf2, index = c("zip", "month"), model = "within")

summary(ft2)

coeftest(ft2, vcov. = vcovHC, type = "HC1")
