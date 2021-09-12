library(anytime)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(anytime)
library(scales)

#LOAD DATA
house <- read.csv('Data/kc_house_data.csv')
head(house)

#DATA SPECIFICATION
dim(house)     # dimention of the data frame
str(house)      # structure
summary(house)  # statistical summary
length(house)   # no. of columns in the data set
colnames(house)   # name of columns
View(house)

#COLUMNS DESCRIPTION (PL)
# date - Data sprzedazy
# price -Cena sprzedazy
# bedrooms - Ilosc sypialni
# bathrooms - Liczba lazienek
# sqft_living - Powierzchnia mieszkalna
# sqft_lot - Powierzchnia ogólna
# floors - Liczba pieter
# waterfront - Czy mieszkanie wychodzilo na nabrzeze czy nie
# condition - Kondycja budnku w skali 1-5
# grade - Ocena w skali 1-13 okreslajaca jak dobrze zostal wykonany budynek
# sqft_above - Powierzchnia wewnętrznej przestrzeni mieszkalnej nad poziomem gruntu
# basement - Czy dom posiada piwnice 0/1
# yr_built - Rok budowy
# yr_renovated - Czy dom byl poddany renowacji 0/1
# zipcode - Kod pocztowy
# lat - Szerokość geo.
# long - Długość geo.
# sqft_living15 - Powierzchnia wnętrza mieszkań dla najbliższych 15 sąsiadów
# sqft_lot15 - Powierzchnia wnętrza mieszkań dla najbliższych 15 sąsiadów

#TRANSFORM DATA
df <- data.frame(house)
df <- subset(df, select = -c(id, view))

df$basement <- 0
df$basement[df$sqft_basement > 0] <- 1
df$renovated <- 0
df$renovated[df$yr_renovated > 0] <- 1
df <- subset(df, select = -c(sqft_basement, yr_renovated))

df$date <- anydate(df$date)
df[order(df$date),]
df$bathrooms <- round(df$bathrooms)
df$floors <- round(df$floors)

df[df$bedrooms < 14,]


df3 <- df %>%
  group_by(sqft_living) %>%
  summarise(avg = mean(price))
str(df3)


#DATA PRESENTATION
dim(df)     # dimention of the data frame
str(df)      # structure
summary(df)  # statistical summary
length(df)   # no. of columns in the data set
colnames(df)   # name of columns
View(df)


########################################################################
########################################################################
ui <- dashboardPage(
  dashboardHeader(title = "House Sales"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Korelacje', tabName = 'tab01', icon = icon('th', lib = 'glyphicon')),
      menuItem('Dashboard', tabName = 'tab02', icon = icon('dashboard')),
      menuItem('Interaktywna mapa', tabName = 'tab03', icon = icon('map-marker', lib = 'glyphicon')),
      menuItem('Tabela', tabName = 'tab04', icon = icon('list', lib = 'glyphicon')),
      menuItem('Wykresy dodatkowe', tabName = 'tab05', icon = icon('signal', lib = 'glyphicon'))
    )),
  dashboardBody(tabItems(
    tabItem(tabName = 'tab01', fluidRow(titlePanel(h1("Macierz korelacji", align = "center")),
                                        plotOutput('plot01', height = 800, width = '100%'))),

    tabItem(tabName = 'tab03', box(sliderInput(inputId = 'inputPlot06', label = h3('Interaktywna mapa cen mieszkań'), width = '90%', min = min(df$price),
                                               max = max(df$price), value = min(df$price)), collapsible = TRUE, width = '100%', height = '150px', align = 'center'),
            box(leafletOutput('plot05', height = '750px'), collapsible = TRUE, width = '100%', height = '750px')),

    tabItem(tabName = 'tab04', fluidRow(titlePanel(h1("Przedstawienie danych w tabeli", align = "center")), dataTableOutput('tabelka'))),
    tabItem(tabName = 'tab02', fluidRow(titlePanel(h1("Dashboard - House Sales in King Country, USA", align = "center")),
                                        box(selectInput(inputId = 'inputPlot02', label = 'Okres czasu:', width = '40%', choices = c('year', 'month', 'day'), selected = 'month'),
                                            plotOutput('plot02'), title = h3("Przedstawienie ilości sprzedanych mieszkań w zależności od okresu")),

                                        box(selectInput(inputId = 'inputPlot03', label = 'Zmienna', choices = c(names(df)[2:5], names(df)[9:10], names(df)[12:12])),
                                            plotOutput('plot03'), title = h3('Ilość sprzedanych domów w zalezności od różnych parametrów')),

                                        box(selectInput(inputId = 'inputPlot04', label = 'Zależność od: ', choices = c(names(df)[5:6], names(df)[12:12], names(df)[17:17])),
                                            selectInput(inputId = 'inputPlot05', label = 'Kolorowanie: ', choices = c(names(df)[3:4], names(df)[7:10], names(df)[18:10])),
                                            plotOutput(outputId = 'plot04'), title = h3('Zależność ceny mieszkać w zależności od różnych czynników')),


                                        box(selectInput(inputId = 'inputPlot07', label = 'Wybierz zmienną: ', choices = c(names(df)[3:4], names(df)[7:7], names(df)[10:10])),
                                            plotOutput(outputId = 'plot08'), title = h3('Zależność ceny mieszkać w zależności od różnych czynników')))),

    tabItem(tabName = 'tab05', fluidRow(titlePanel(h1("Wykresy dodatkowe", align = "center")),

                                        box(selectInput(inputId = 'inputPlot08', label = 'Zmienne', choices = c('bedrooms', 'bathrooms', 'floors', 'condition', 'basement', 'renovated')),
                                            plotOutput(outputId = 'plot09'), title = h3('Procentowy udział mieszkań o różnych parametrach')),

                                        box(sliderInput(inputId = 'sliderInput10', label = 'przedzial od', max = max(df3$avg), min = min(df3$avg), value = min(df3$avg)),
                                            sliderInput(inputId = 'sliderInput11', label = 'przedzial do', max = max(df3$avg), min = min(df3$avg), value = max(df3$avg)),
                                            plotOutput(outputId = 'plot10'), title = h3('Zmiana powierzchni mieszkalnej w zaleznosci od ceny'))))

  )),
  skin = "green"

)


server <- function(input, output) {
  output$plot01 <- renderPlot({
    M <- cor(subset(df, select = -c(date, floors, waterfront, grade, condition, zipcode)))
    corrplot.mixed(M, lower.col = "black", number.cex = .7)
  })
  output$plot02 <- renderPlot({
    if (input$inputPlot02 == 'year')
      plot02 <- ggplot(df, aes(x = format(date, format = "%Y")))
    else
      if (input$inputPlot02 == 'month')
        plot02 <- ggplot(df, aes(x = format(date, format = "%m")))
      else
        if (input$inputPlot02 == 'day')
          plot02 <- ggplot(df, aes(x = format(date, format = "%d")))

    plot02 +
      geom_bar(fill = 'cyan', color = 'black') +
      xlab(input$inputPlot02) +
      ylab('Ilość sprzedanych mieszkań') +
      theme_bw()


  })


  output$tabelka <- renderDataTable(df)

  output$plot03 <- renderPlot({
    ggplot(df, aes(unlist(df[input$inputPlot03]))) +
      geom_bar(color = 'black', fill = 'green') +
      xlab(input$inputPlot03) +
      ylab("Ilość wystąpień") +
      theme_bw()

  })

  output$plot04 <- renderPlot({
    chart <- ggplot(df, aes(x = unlist(df[input$inputPlot04]), y = price, color = unlist(df[input$inputPlot05]))) +
      geom_point() +
      xlab(input$inputPlot04) +
      ylab("price") +
      labs(color = input$inputPlot05)
    theme_bw()


    chart + scale_color_gradient(low = "blue", high = "red")

  })

  output$plot05 <- renderLeaflet({

    dfcoppy <- df

    dfcoppy$priceParts[as.numeric(dfcoppy$price) < 250000] <- 'a) < 250tys$'
    dfcoppy$priceParts[as.numeric(dfcoppy$price) >= 250000 & as.numeric(dfcoppy$price) < 500000] <- 'b) 250-500tys$'
    dfcoppy$priceParts[as.numeric(dfcoppy$price) >= 500000 & as.numeric(dfcoppy$price) < 750000] <- 'c) 500-750tys$'
    dfcoppy$priceParts[as.numeric(dfcoppy$price) >= 750000 & as.numeric(dfcoppy$price) < 1000000] <- 'd) 750tys-1mln$'
    dfcoppy$priceParts[as.numeric(dfcoppy$price) >= 1000000 & as.numeric(dfcoppy$price) < 2000000] <- 'e) 1-2mln$'
    dfcoppy$priceParts[as.numeric(dfcoppy$price) >= 2000000] <- 'f) > 2mln $'


    center_lon <- median(dfcoppy$long, na.rm = TRUE)
    center_lat <- median(dfcoppy$lat, na.rm = TRUE)

    dfcoppy <- dfcoppy[order(dfcoppy$priceParts),]
    factpal <- colorFactor(c("black", "blue", "yellow", "orange", "cyan", "red"),
                           as.factor(c('a) < 250tys$', 'b) 250-500tys$', 'c) 500-750tys$', 'd) 750tys-1mln$', 'e) 1-2mln$', 'f) > 2mln $')),
    )


    leaflet(dfcoppy[input$inputPlot06 <= dfcoppy$price,]) %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      addCircles(lng = ~long, lat = ~lat,
                 color = ~factpal(priceParts)) %>%

      # controls
      setView(lng = center_lon, lat = center_lat, zoom = 12) %>%

      leaflet::addLegend("bottomright", pal = factpal, values = ~priceParts,
                         title = "House Price Distribution",
                         opacity = 1)

  })

  output$plot08 <- renderPlot({

    ggplot(df, aes(as.factor(unlist(df[input$inputPlot07])), log(price), fill = factor(unlist(df[input$inputPlot07])))) +
      geom_boxplot(alpha = 0.5) +
      scale_y_continuous(breaks = seq(0, 16, by = 1)) +
      stat_summary(fun = mean, geom = "point", size = 1, color = "red") +
      theme(legend.position = "none") +
      xlab(input$inputPlot07) +
      ylab("price")


  })


  output$plot09 <- renderPlot({
    df2 <- df
    colnames(df2)[colnames(df2) == input$inputPlot08] <- 'column'
    print(df2)

    df2 <- df2 %>%
      group_by(column) %>%
      summarise(count = n())



    df2$column <- as.factor(df2$column)


    chart1 <- ggplot(df2, aes(x = "", y = count, fill = column)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = ifelse(percent(count / sum(count), 0.1) > 1,
                                   percent(count / sum(count), 0.1), ""),
                    x = 1.7), size = 6, show.legend = F, position = position_stack(vjust = 0.5)) +
      labs(fill = input$inputPlot08) +
      theme_void()

    chart1

  })

  output$plot10 <- renderPlot({

    if (input$sliderInput10 > input$sliderInput11) {
      updateSliderInput(inputId = 'sliderInput10', value = floor(input$sliderInput11))
    }


    df4 <- df3[input$sliderInput10 <= df3$avg & df3$avg <= input$sliderInput11,]

    try(
      ggplot(data = df4, aes(x = sqft_living)) +
        geom_line(aes(y = avg, colour = "sqft_living"), linetype = 'solid', size = 1) +
        labs(x = "powierzchnia mieszkalna", y = "price") +
        scale_colour_manual("", breaks = "sqft_living", values = "orange") +
        theme_bw(), silent = TRUE)


  })

}

shinyApp(ui, server)




