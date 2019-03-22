yourPath <- "/Users/catlady4lyfe/mymanycsvs/"
allthedata <- data.frame()
for(year in 2000:2018){
  print(year)
  filename <- paste(yourPath, year, "your-file-name.csv", sep='')
  print(filename)
  df <- read.csv(filename, header = TRUE)
  print(df)
  df <- cbind(df, "Year" = year)
  allthedata <- rbind(allthedata, df)
}

write.csv(allthedata, file = "allthedata.csv")