###################
## test for date
###################

## testDate <- head(traume$inj_start_date)
set.seed(123)
testDate <- sample(traume$inj_start_date, 10)
length(testDate)
str(testDate)
testDate[c(5,8)] <- c("11.11.2018 12:03:20", "12.06.2017 09:30:50")

grep(".*00:00:00$", testDate)
length(grep(".*00:00:00$", testDate))

grepl(".*00:00:00$", testDate)
sum(grepl(".*00:00:00$", testDate))
sum(grepl(".*00:00:00$", testDate) == TRUE)
sum(grepl(".*00:00:00$", testDate) == FALSE)

grep("\\d{2}.\\d{2}.\\d{4} 12:03:\\d{2}", testDate)
## grep klokke og minutt
grep(" [0-2][0-9]:\\d{2}:\\d{2}$", x = testDate, value = TRUE)

###############
## ukenummer ##
###############
## konvertere til dato
testDate2 <- as.Date(testDate, format = "%d.%m.%Y %H:%M:%S") #beholder ikke tid og minutt
testDate3 <- as.POSIXct(testDate, format = "%d.%m.%Y %H:%M:%S")
testDate4 <- as.POSIXlt(testDate, format = "%d.%m.%Y %H:%M:%S")
## henter ukenummer
strftime(testDate2, format = "%V") #ukerdager som i ISO 8601
strftime(testDate3, format = "%V")
strftime(testDate4, format = "%a")



test <- 1:10

i <- 0
while (i <= 5) {
  print(i)
  i <- i + 1
}
