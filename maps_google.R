library(ggmap)
library(stringr)
register_google(key = "AIzaSyBCQN8_QIAvVpeT6zeslqVR6Y71nR7V94U", account_type = "standard")
#Importing Data
df1 <- Code_01A[-1,c(2:4)]
df1 <- df1[complete.cases(df1),]

#Extracting Zip Codes
daily_limit <- 2500
out_mat <- matrix(data = 0, nrow = 9514, ncol = 1)

for(j in 1:9514){
  
  cc <- as.numeric(df1[j,c(1:2)])
  out <- revgeocode(rev(cc), output = "address", force = TRUE)
  x <- unlist(strsplit(out, "IL"))
  x <- unlist(strsplit(x[2], "USA"))
  x <- unlist(strsplit(x, ","))
  
  out_mat[j,1] <- as.numeric(x[1]) 
}

#Analyzing Crime Code 01A for each Zip Code
zill_zip <- zill_dat2[,1]
indx_zip <- unique(zip_com)
tda <- time_dat[c(1:2500),2]
tda <- unlist(tda)
id <- which(zill_zip == 60624)

ptoc <- unlist(zill_dat1[id,c(2:101)])
mon_year <- names(ptoc)

#Find the month-year for crimes in Zillow data
crime_zill <- sort(unlist(intersect(tda,mon_year)))
ptoc <- ptoc[crime_zill]

plot(ptoc, type = "b", xaxt = "n", col = "blue")
axis(1, at=factor(crime_zill), crime_zill)

A <- intersect(z1,z2)
B <- intersect(A,z3)
C <- intersect(B,z4)


#Analysis for 60651
pt1 <- unlist(zill_dat1[which(zill_dat1[,1]==60651),c(2:101)])
pt2 <- unlist(zill_dat2[which(zill_dat2[,1]==60651),c(2:101)])
pt3 <- unlist(zill_dat3[which(zill_dat3[,1]==60651),c(2:100)])
pt4 <- unlist(zill_dat4[which(zill_dat4[,1]==60651),c(2:102)])[2:101]

pt1 <- pt1[crime_zill]
pt2 <- pt2[crime_zill]
pt3 <- pt3[crime_zill]
pt4 <- pt4[crime_zill]

plot(pt4, type = "b", xaxt = "n", col = "blue", xlab = "Year-Month", ylab = "Percentage")
axis(1, at=factor(crime_zill), crime_zill)