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


## legend manually
ggplot(df, aes(x = instance, y = total_hits))+geom_point(size = 1)+geom_line()+
  geom_line(aes(x=instance, y = line1, colour="myline1")) +
  geom_vline(xintercept=805)+geom_line(aes(x=df$instance, y = line2, colour="myline2"))+
  geom_line(aes(x=instance, y = line3, colour="myline3")) +
  scale_colour_manual(name="Line Color",
                      values=c(myline1="red", myline2="blue", myline3="purple"))
## option 2
ggplot(d, aes(x)) +
  geom_line(aes(y=y1, colour="1")) +
  geom_line(aes(y=y2, colour="2")) +
  scale_colour_manual(values=c("red", "blue"))


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
## time - series
require(xts)
stockprices.ts <- xts(stockprices$prices, order.by=as.POSIXct(stockprices$timestamps))
stockprices.ts

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


############################
## Shiny subset
#server
library(shiny)

ui2 <- fluidPage(
  #User dropbox
  selectInput("state", "Choose state", choices=c("MA", "CA", "NY"))
  #Print table to UI
 ,tableOutput("table1")
)

server2 <- function(input,output){

  category <- c("MA", "CA", "NY")
  population <- c(3,8,4)

  df <- data.frame(category,population)

  df_subset <- reactive({
    a <- subset(df, category == input$state)
    return(a)
  })

  output$table1 <- renderTable(df_subset()) #Note how df_subset() was used and not df_subset

  ## observe({ df_subset <- get(input$state)})

  ## output$table1 <- df_subset
}

shinyApp(ui = ui2, server = server2)


############################
## Dynamic input
############################
library(data.table)
data <- fread("~/avid/ntr/data/ReshHF.csv", encoding = "Latin-1")

valgRHF <- "HSØ"
valgHF <- "OUS"
subData <- data[RHF == valgRHF, ]
subData2 <- data[, `:=`(valgEnhet = ifelse(HF == valgHF, 1, 0))]

## data[, unique(RHF)]


library(shiny)

ui3 <- fluidPage(

  headerPanel("NTR Test"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("valgEnhet"),
      uiOutput("valgEnhetNavn"),

      checkboxInput("compare", "Sammenligne med hele landet")
    ),
    mainPanel(

    )
  )
)

helseEnhet <- c("RHF", "HF", "Sykehus")

ser3 <- function(input, output, session) {

  output$valgEnhet <- renderUI({
    radioButtons("enhet", "Valg Enheten",
                 choices = as.list(helseEnhet),
                selected = NULL)
  })

  output$valgEnhetNavn <- renderUI({
    # if missing input, return to avoid error
    if(is.null(input$enhet)) return()

    valgetEnhet <- input$enhet
    enhetNavn <- data[, sort(unique(get(valgetEnhet)))]

    selectInput("helseNavn", "Helse Enheten",
                choices = enhetNavn)

  })

}

shinyApp(ui = ui3, server = ser3)
