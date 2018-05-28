## Forslag til analyser
#######################################
## Antall hver aldersgruppe per kjønn
#######################################
traume[PatientAge == -1, PatientAge := NA] #bytt -1 til NA
## finner ut hvordan velger man -1 f.eks PatientAge!=-1 ikke funker. Koden nedenfor eksluderer ikke -1
## demo <- traume[PatientAge > -1 | !is.na(PatientAge) , .N, keyby = list(PatientGender, PatientAge)]

demo <- traume[!is.na(PatientAge) , .N, keyby = list(PatientGender, PatientAge)]

## Summen for hver alderskategori
demo[, Sum := sum(N), by = list(PatientAge)]

library(dygraphs)
ageMale <- demo[PatientGender == 1, list(PatientAge, nm = N)]
ageFemale <- demo[PatientGender == 2, list(PatientAge, nf = N)]
##ageMF <- cbind(ageMale, ageFemale)
ageMF <- merge(ageMale, ageFemale, by.x = "PatientAge", by.y = "PatientAge")
ageMF[, tot := nm + nf]

dygraph(ageFemale)
ageDemo <- ggplot(ageMF, aes(x = PatientAge)) +
  geom_line(aes(y = nm), color = "blue", size = 1.5) +
  geom_line(aes(y = nf), color = "red", size = 1.5) +
  geom_line(aes(y = tot), color = "green", size = 1.5) +
  ## title(xlab = "Alder", ylab = "Antall") +
  xlab("Alder") +
  ylab("Antall")

library(plotly)
ggplotly(ageDemo)

library(ggplot2)
ggplot(demo, aes(as.factor(PatientAge), N, group = PatientGender, color = as.factor(PatientGender))) +
  geom_line() + theme_linedraw()

ggplot(demo) +
  geom_line(aes(as.factor(PatientAge), N, group = PatientGender, color = as.factor(PatientGender))) +
  geom_line(aes(as.factor(PatientAge), Sum))

## med sum linje
ggplot(beav1, aes(x=time, y=temp)) +
  geom_line(aes(group=day, fill=day, color=day))+
  stat_summary(fun.y = mean, na.rm = TRUE, group = 3, color = 'black', geom ='line')

#################
## plotly
#################
library(plotly)
## with points
dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

# Map sex to different point shape, and use larger points
p <- ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
  geom_line() +
  geom_point()

p <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="geom_line/larger")
chart_link

## style and theme
p <- ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) +
  geom_line(aes(linetype=sex), size=1) +     # Set linetype by sex
  geom_point(size=5) +         # Use larger points, fill with white
  scale_colour_hue(name="Sex",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Sex",
                     values=c(22,21)) +      # Use points with a fill color
  scale_linetype_discrete(name="Sex") +
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw()

p <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="geom_line/themes")
chart_link


################
## Dygraphs
################
library(dygraphs)
##library(magrittr)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)
##2
dygraph(lungDeaths) %>% dyRangeSelector()

## 3
dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

## 4
hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

## highlighting
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)
