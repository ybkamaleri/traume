library(plotly)

test_data <-
  data.table(
    var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
    var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
    date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
  )

test_data_long <- data.table::melt(test_data, id.vars=c("date"))  # convert to long format

p <- ggplot(data=test_data_long,
            aes(x=date, y=value, colour=variable)) +
  geom_line()

p <- ggplotly(p)
p

### DT melt
s1 <- "family_id age_mother dob_child1 dob_child2 dob_child3
1         30 1998-11-26 2000-01-29         NA
2         27 1996-06-22         NA         NA
3         26 2002-07-11 2004-04-05 2007-09-02
4         32 2004-10-10 2009-08-27 2012-07-21
5         29 2000-12-05 2005-02-28         NA"
DT <- fread(s1)
DT

DT.m1 = melt(DT, id.vars = c("family_id", "age_mother"),
             measure.vars = c("dob_child1", "dob_child2", "dob_child3"))
DT.m1
str(DT.m1)

test_data

######################################
## Simple plot
set.seed(112)
dt <- data.table(id = 1:30, gender = sample(1:2, 30, replace = T), age = sample(15:60, 30, replace = T))
dt

#### sequence and cut
a <- 1:10
#include 1 og exclude 6 i kategorien
cut(a, breaks = c(seq(1,60,5), Inf), include.lowest = TRUE, right = F)
# Standard måte - her vises at 1 er ikke inkludert i kategorien
cut(a, breaks = c(seq(1,60,5), Inf))

## Alder kategorisering
## Alder kategori
alder.kat2 <- function(x, lower, upper, by,
                       sep = "-") {
  labs <- paste0(c(paste(seq(lower, upper - by, by = by),
                         seq(lower + by - 1, upper - 1, by = by),
                         sep = sep),
                   paste(upper, "+", sep = "")), " år")
  cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      include.lowest = TRUE, right = FALSE, labels = labs)
}

library(ggplot2)
dtSel <- dt
dtMan <- dtSel[!is.na(gender) == 1 & !is.na(age) & age != -1, .(mann = .N), key = age]
dtMan
(ageMax <- max(dtMan$age, na.rm = TRUE))
(ageMin <- min(dtMan$age, na.rm = TRUE))
dtCopy <- copy(dtSel)
(dtCopy[gender == 1, ageK := alder.kat2(age, ageMin, ageMax, 5)])
dtCopy
[gender == 1, .(mann = .N), key = ageK]
dtMan2

dtKvinne <- dt[!is.na(gender) == 2 && n == 1, .(kvinne = .N), key = age]
ageKMax <- max(dtKvinne, na.rm = TRUE)
dtKvinne2 <- dtCopy[n == 1, ageK := alder.kat2(age, 0, ageKMax, 5)][n == 1 && gender == 2, .(kvinne = .N), key = ageK]
dtMK <- merge(dtMan2, dtKvinne2, all = TRUE)

library(microbenchmark)
microbenchmark(
  a <- for (ind in seq_len(ncol(dtMK))) set(dtMK, i = which(is.na(dtMK[[ind]])),j = ind, value = 0),
  b <- for (ind in seq_along(dtMK)) set(dtMK, i = which(is.na(dtMK[[ind]])),j = ind, value = 0) #start nummer
)

bNA(dtMK)
dtMK[is.na(dtMK)] <- 0
dtMK

dtMK[, alle := mann + kvinne, by = ageK]

dtMK2 <- melt(dtMK, id.vars = "ageK",
              measure.vars = c("mann", "kvinne", "alle"),
              variable.name = "sex",
              value.name = "N")
dtMK2

library(ggplot2)

(ggplot(dtMK2, aes(ageK, N, color = sex, group = sex)) +
   geom_line())
