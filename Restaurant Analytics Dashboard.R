#REMOVE LATER
setwd("/Users/zijingohmeywu/Downloads/recruit-restaurant-visitor-forecasting 2/")

library('leaflet')
library('dplyr')
library('lubridate')
library('shiny')
library('tidyverse')
library('plotly')
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('ggrepel') # visualisation
library('ggExtra') # visualisation
library('viridis') # visualisation
library('ggridges')
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('shinyWidgets')
library(readr)
library(gganimate)

wday <- lubridate::wday

#SAALIM ---------------------------------------------------------------------------------------------------------
saalim_visit <- read.csv("air_visit_data.csv", stringsAsFactors = FALSE)
saalim_info <- read.csv("air_store_info.csv", stringsAsFactors = FALSE)

saalim_Data_Full <- merge(saalim_visit,saalim_info, by = "air_store_id")

saalim_info_id <- data.frame(saalim_info$air_store_id)
saalim_info_genre <- data.frame(saalim_info$air_genre_name)
saalim_info_air_area_name <- data.frame(saalim_info$air_area_name)
saalim_info_longitude <- data.frame(saalim_info$longitude)
saalim_info_latitude <- data.frame(saalim_info$latitude)
saalim_info_g <- data.frame(cbind(saalim_info_id,saalim_info_genre,saalim_info_air_area_name,saalim_info_longitude,saalim_info_latitude))
colnames(saalim_info_g)[1] <- "air_store_id"
colnames(saalim_info_g)[2] <- "air_genre_name"
colnames(saalim_info_g)[3] <- "air_area_name"
colnames(saalim_info_g)[4] <- "longitude"
colnames(saalim_info_g)[5] <- "latitude"
saalim_visit_full <- merge(saalim_visit,saalim_info_g, by = "air_store_id")

saalim_trial <- saalim_visit_full
saalim_tmp <- saalim_trial$visit_date
saalim_tmp <- gsub('-', '', saalim_tmp)
saalim_tmp <- ymd(saalim_tmp)
saalim_trial$Month <-month(saalim_tmp)

#rename prefectures
saalim_n_prefects <- data.frame(cbind(saalim_Data_Full$air_store_id, saalim_Data_Full$air_area_name))
saalim_X2 <- data.frame(saalim_n_prefects$X2)
saalim_new_prefects <- separate(saalim_X2, 1, into = c('A','B','C','D','E'), sep = '\\s+')
saalim_prefects <- data.frame(saalim_new_prefects$A)
saalim_prefects <- data.frame(cbind(saalim_n_prefects$X1, saalim_prefects))
colnames(saalim_prefects)[1] <- "air_store_id"
saalim_trial$air_area_name <- saalim_prefects$saalim_new_prefects.A

#find avg no. of visitors per day grouped by restaurant genre
saalim_trial <- group_by(saalim_trial, air_genre_name, air_area_name, visit_date)
saalim_trial <- mutate(saalim_trial, avg_visitors = mean(visitors))

#[END SAALIM]---------------------------------------------------------------------------------------------------------------------

#OHMEY----------------------------------------------------------------------------------------------------------------
air_visitsO <- read.csv("air_visit_data.csv", header = TRUE, stringsAsFactors = FALSE)
air_reserveO <- read.csv("air_reserve.csv", header = TRUE, stringsAsFactors = FALSE)
air_storeO <- read.csv("air_store_info.csv", header = TRUE, stringsAsFactors = FALSE)
store_idsO<- read.csv("store_id_relation.csv", header = TRUE, stringsAsFactors = FALSE)
testO <- read.csv("sample_submission.csv", header = TRUE, stringsAsFactors = FALSE)


## Reformating features

air_visitsO <- air_visitsO %>%
  mutate(visit_date = ymd(visit_date))

air_reserveO <- air_reserveO %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))
air_storeO <- air_storeO %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

fooO <- air_reserveO %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE, week_start = 1),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE, week_start = 1),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

fooO %>%
  arrange(desc(diff_day)) %>%
  select(reserve_datetime, visit_datetime, diff_day, air_store_id)


fooO <- air_visitsO %>%
  rename(date = visit_date) %>%
  distinct(date) %>%
  mutate(dset = "train")

barO <- testO %>%
  separate(id, c("fooO", "bar", "date"), sep = "_") %>%
  mutate(date = ymd(date)) %>%
  distinct(date) %>%
  mutate(dset = "testO")

fooO <- fooO %>%
  bind_rows(barO) %>%
  mutate(year = year(date))
year(fooO$date) <- 2017

foo <- air_reserveO %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(air_store_id,visit_date) %>%
  summarise(reserve_visitors_air = sum(reserve_visitors))

all_reserve <- air_visitsO %>%
  inner_join(foo, by = c("air_store_id", "visit_date"))

fooO <- air_visitsO %>%
  left_join(air_storeO, by = "air_store_id")

filterfoo <- fooO %>%
  mutate(wday = wday(visit_date, label = TRUE, week_start = 1)) %>%
  group_by(wday, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) 

p <- all_reserve %>%
  filter(reserve_visitors_air < 120) %>%
  ggplot(aes(visitors, reserve_visitors_air)) +
  geom_point(color = "dodgerblue4", alpha = 0.6, size = 3)
p <- ggMarginal(p, type="histogram", fill = "dodgerblue2", bins=80)
#                            --------END OF Visitors vs Days & Vistor Density vs Days--------
#[END OHMEY]----------------------------------------------------------------------------------------------------------
#[START KING]-----------------------------------------------------------------------------------------------------

king_air_reserve <- read_csv("air_reserve.csv")
king_air_store_info <- read_csv("air_store_info.csv")
king_air_visit_data <- read_csv("air_visit_data.csv")

king_merged_data <- merge(king_air_store_info, king_air_visit_data, by = "air_store_id")

#remove unnecessary genres
king_d<-king_merged_data

#create monthcol
king_d <- king_d %>%
  mutate(month(visit_date))
##### tab specific set up
#4a
king4a_tmp <- king_d
king4a_tmp <-king4a_tmp %>%
  mutate(day(visit_date)) %>%
  mutate(year(visit_date)) %>%
  mutate(word(air_area_name, 1)) %>%
  group_by(day(visit_date), month(visit_date), air_genre_name, word(air_area_name, 1), year(visit_date)) %>%
  summarize(totalvisit = mean(visitors))

colnames(king4a_tmp)[1]<-'day'
colnames(king4a_tmp)[2] <- 'month'
colnames(king4a_tmp)[4] <- 'prefecture'
colnames(king4a_tmp)[5] <- 'year'

king4a_tmp$prefecture <- iconv(king4a_tmp$prefecture, to='ASCII//TRANSLIT') %>%
  str_replace_all('AO', 'O') %>%
  str_replace_all('A\\?', 'o')

#list of choices for ui
king4a_list <- unique(king4a_tmp$prefecture) %>% 
  word(1) %>%
  unique()

###########
#4b
king4b_tmp <- king_d

king4b_tmp$prefecture <- king4b_tmp$air_area_name %>%
  word(1) %>%
  iconv(to='ASCII//TRANSLIT') %>%
  str_replace_all('AO', 'O') %>%
  str_replace_all('A\\?', 'o')

#list of choices for ui
king4b_list <- unique(king4b_tmp$prefecture) %>% 
  word(1) %>%
  unique()

#############
#4c
king4c_tmp <- king_d
king4c_tmp <-king4c_tmp %>%
  mutate(word(air_area_name, 1)) 

colnames(king4c_tmp)[9] <- 'prefecture'
#remove special characters
king4c_tmp$prefecture <- iconv(king4c_tmp$prefecture, to='ASCII//TRANSLIT') %>%
  str_replace_all('AO', 'O') %>%
  str_replace_all('A\\?', 'o')

king4c_list <- unique(king4c_tmp$prefecture) %>% 
  word(1) %>%
  unique()

##########
#2a
king2a_tmp <-king_d %>%
  mutate(day(visit_date)) %>%
  mutate(paste(year(visit_date), quarter(visit_date))) %>%
  group_by(visit_date, day(visit_date), paste(year(visit_date), quarter(visit_date))) %>%
  summarize(totalvisit = mean(visitors))

king2a_tmp$newcol <- format(as.Date(king2a_tmp$visit_date), "%b-%d")
colnames(king2a_tmp)[3] <- 'quarteryear' 

#######
#[END KING]---------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("What You Didn't Know About Restaurants"),
  navbarPage("Navigation",
             navbarMenu("Filtered",
                        tabPanel("Median visitors",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     #####
                                     checkboxGroupInput(inputId="genre",  h3("Please select Genre"),
                                                        choices = unique(king4a_tmp$air_genre_name), selected = "Bar/Cocktail"),
                                     
                                     checkboxGroupInput(inputId = "prefecture",
                                                        h3("Please select Prefecture"),
                                                        choices = king4a_list, selected="Fukuoka-ken"),
                                     
                                     sliderTextInput(inputId = "month",
                                                     label = "Month:",
                                                     choices = c(1:12)),
                                     sliderTextInput(inputId = "year",
                                                     label = "Year:",
                                                     choices = c('2016', '2017'))
                                     
                                   ),
                                   mainPanel(
                                     plotlyOutput("plot4a", height=700))
                                 )),
                        tabPanel("median visitors by months",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     
                                     checkboxGroupInput(inputId="genreB",  h3("Please select Genre"),
                                                        choices = unique(king4a_tmp$air_genre_name), selected = "Bar/Cocktail"),
                                     
                                     checkboxGroupInput(inputId = "prefectureB",
                                                        h3("Please select Prefecture"),
                                                        choices = king4a_list, selected="Fukuoka-ken")),
                                   
                                   
                                   
                                   mainPanel(
                                     plotOutput("plot4b", height=700))
                                 )),
                        tabPanel("filtered visitor frequency",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons(inputId="genreC", label="genre:",
                                                  choices = unique(king4c_tmp$air_genre_name)),
                                     
                                     radioButtons(inputId = "prefectureC",
                                                  label = "prefecture:",
                                                  choices = king4c_list)
                                   ),
                                   mainPanel(
                                     plotlyOutput("plot4c", height=700))
                                 ))
             ),
             navbarMenu("Aggregate",
                        tabPanel("Quarterly plot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderTextInput(inputId = "quarter", 
                                                     label = "Quarter-year:", 
                                                     choices = c('2016 1', '2016 2', '2016 3', '2016 4', '2017 1', '2017 2'))
                                   ),
                                   mainPanel(
                                     plotOutput("plot2a", height=700))
                                 )),
                        
                        tabPanel("Day of Week plot",
                                 
                                 mainPanel(
                                   plotlyOutput("plot2c", height=700))
                        ),
                        
                        tabPanel("Aggregate visitor frequency",
                                 
                                 mainPanel(
                                   plotOutput("plot2d", height=700))
                        )
                        
             ),
             ####
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("saalim_genre", h3("Genre"),
                                             choices=setNames(object=c("Bar/Cocktail","Cafe/Sweets",
                                                                       "Creative cuisine","Dining bar",
                                                                       "Italian/French","Izakaya",
                                                                       "Japanese food",
                                                                       "Okonomiyaki/Monja/Teppanyaki",
                                                                       "Other","Western food" ,
                                                                       "Yakiniku/Korean food", "Asian",
                                                                       "International cuisine", "Karaoke/Party"),
                                                              nm=c("Bar/Cocktail","Cafe/Sweets",
                                                                   "Creative cuisine","Dining bar",
                                                                   "Italian/French","Izakaya",
                                                                   "Japanese food",
                                                                   "Okonomiyaki/Monja/Teppanyaki",
                                                                   "Other","Western food" ,
                                                                   "Yakiniku/Korean food", "Asian",
                                                                   "International cuisine", "Karaoke/Party")),
                                             selected = c("Bar/Cocktail","Cafe/Sweets",
                                                          "Creative cuisine","Dining bar",
                                                          "Italian/French","Izakaya",
                                                          "Japanese food",
                                                          "Okonomiyaki/Monja/Teppanyaki",
                                                          "Other","Western food" ,
                                                          "Yakiniku/Korean food", "Asian",
                                                          "International cuisine", "Karaoke/Party"))
                        ),
                        
                        mainPanel(
                          leafletOutput("saalim_mymap", height=1000),
                          absolutePanel(top = 10, right = 10,
                                        style="z-index:500;", # legend over my map (map z = 400)
                                        tags$h3("Restaurants in Japan"),
                                        sliderInput("saalim_month", "Time", min = as.Date("2016-01-01","%Y-%m-%d"),
                                                    max =as.Date("2017-04-22","%Y-%m-%d"),
                                                    value=as.Date("2016-01-01"),
                                                    timeFormat="%Y-%m-%d",
                                                    animate = animationOptions(interval = 1000,loop = TRUE))
                          )))),
             tabPanel("Genre",
                      tabPanel("Visitor vs Genre",
                               sidebarLayout(
                                 sidebarPanel(
                                   checkboxGroupInput("hahawday", h3("Day of the Week"),
                                                      choices=setNames(object=c("Mon","Tue",
                                                                                "Wed","Thu",
                                                                                "Fri","Sat",
                                                                                "Sun"),
                                                                       nm=c("Mon","Tue",
                                                                            "Wed","Thu",
                                                                            "Fri","Sat",
                                                                            "Sun")),
                                                      selected = c("Mon","Tue",
                                                                   "Wed","Thu",
                                                                   "Fri","Sat",
                                                                   "Sun"))
                                 ),
                                 
                                 mainPanel(
                                   plotOutput("p1", height=1000)
                                 ))),
                      
                      
                      tabPanel("Visitor Density",
                               sidebarLayout(
                                 sidebarPanel(
                                   checkboxGroupInput("hahawday2", h3("Day of the Week"),
                                                      choices=setNames(object=c("Mon","Tue",
                                                                                "Wed","Thu",
                                                                                "Fri","Sat",
                                                                                "Sun"),
                                                                       nm=c("Mon","Tue",
                                                                            "Wed","Thu",
                                                                            "Fri","Sat",
                                                                            "Sun")),
                                                      selected = c("Mon","Tue",
                                                                   "Wed","Thu",
                                                                   "Fri","Sat",
                                                                   "Sun"))
                                 ),
                                 mainPanel(
                                   plotOutput("p2", height=1000)
                                 )))),
             
             tabPanel("Reservations",
                      mainPanel(width = 12, style = "border-style: solid; border-color: black",
                                plotOutput("p", height = "1000px", width = "100%")
                      ))
  ))

server <- function(input, output) {
  #Visitor vs Days of Week
  reactive_set1 <- reactive({
    filterfoo %>%
      filter(wday %in% input$hahawday)
  })
  #Viitor vs Days of Week
  output$p1 <- renderPlot({
    ggplot(data = reactive_set1(), aes(air_genre_name, mean_visitors, color = wday)) +
      geom_point(size = 5) + theme(legend.position = "left",
                                   plot.title = element_text(size = 14)) + coord_flip() +
      labs(x = "") + scale_x_discrete(position = "top") + ggtitle("Mean Visitors per Genre") +
      scale_color_hue()
  })
  
  #Visitor Density
  reactive_set2 <- reactive({
    filterfoo %>%
      filter(wday %in% input$hahawday2)
  })
  #Visitor Density
  output$p2 <- renderPlot({
    ggplot(data = reactive_set2(), aes(mean_visitors, air_genre_name, fill = air_genre_name)) +
      geom_density_ridges(bandwidth = 0.1) + scale_x_log10() + labs(y = "") + labs(x = "Distribution of Visitors") + scale_fill_cyclical(values = c("brown4", "brown2", "gold1", "yellow", "darkgreen", "forestgreen",  "darkolivegreen2", "dodgerblue3","deepskyblue1","darkorchid4", "darkorchid3")) + ggtitle("Distribution of Visitors for Each Day")
  })
  
  #Reservations vs Visits
  output$p<- renderPlot({
    p <- all_reserve %>%
      filter(all_reserve$reserve_visitors_air < 120) %>%
      ggplot(aes(visitors, all_reserve$reserve_visitors_air)) +
      geom_point(color = "dodgerblue4", alpha = 0.6, size = 3)
  })
  
  
  reactive_data_chrono <- reactive({
    saalim_trial %>%
      filter(visit_date == input$saalim_month, air_genre_name %in% input$saalim_genre)
  })
  
  
  # static backround map
  output$saalim_mymap <- renderLeaflet({
    leaflet(saalim_trial) %>%
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })  
  
  # reactive circles map
  observe({
    leafletProxy("saalim_mymap", data = reactive_data_chrono()) %>%
      clearShapes() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(radius = 2, color = "red", label = ~visitors,
                       labelOptions = labelOptions(noHide = TRUE, 
                                                   offset=c(0,-12), textOnly = TRUE),
                       options = markerOptions(visitors = ~visitors),
                       clusterOptions = markerClusterOptions(),
                       fillOpacity = 0.5)
  })
  
  output$plot4a <- renderPlotly({
    
    data <- reactive({king4a_tmp %>%
        filter(air_genre_name %in% input$genre & prefecture %in% input$prefecture) %>% 
        filter(month == input$month & year == input$year)
    })
    
    
    fig <- data() %>%
      plot_ly(
        x = ~day,
        y = ~totalvisit,
        frame = ~month,
        type = 'bar',
        mode = 'markers',
        showlegend = F,
        hovertemplate = paste('Day: %{x}', '<br>Total Visit: %{y}<br>')
      )
    
    ##x axis
    X <- list(
      title = paste("Days of month:", month.name[input$month]),
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    ###y axis
    
    Y <- list(
      title = 'Total number of visits',
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    
    fig <- fig %>% layout(xaxis = X, yaxis = Y)
    
    
    fig <- fig %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      )
    fig
    
  })
  
  output$plot4b <- renderPlot({
    
    vectort <- month.abb[c(1:12)]
    
    tmpp2 <- reactive({king4b_tmp %>%
        mutate(monthy=month(visit_date)) %>%
        mutate(month = month.abb[monthy]) %>%
        filter(air_genre_name %in% input$genreB & prefecture %in% input$prefectureB) %>%
        group_by(month, air_genre_name) %>%
        summarise(visits = median(visitors))
    })
    
    tmpp2() %>%
      ggplot(aes(fct_relevel(month, vectort), visits, fill = air_genre_name)) +
      geom_col(position='dodge') +
      labs(x = "Month", y = "Median visitors") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))
    
  })
  
  output$plot4c <- renderPlotly({
    
    
    data <- reactive({
      king4c_tmp %>%
        filter(air_genre_name %in% input$genreC & prefecture %in% input$prefectureC)
    })
    
    fig <- plot_ly(
      x=data()$visitors, type = "histogram", nbinsx = 60 
    )
    
    X <- list(
      title = 'Median visitor count',
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    ###y axis
    
    Y <- list(
      title = "Frequency",
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    
    fig <- fig %>% layout(xaxis = X, yaxis = Y)
    fig
    
  })
  
  output$plot2a <- renderPlot({
    tmpp <- king2a_tmp[king2a_tmp$quarteryear== input$quarter,]
    #  plot(x = tmpp$ratio, y = tmpp$meanrisk)
    tmpp %>%
      ggplot(aes(x=visit_date, y=totalvisit)) +
      geom_col()+
      geom_smooth(se=FALSE) + 
      labs(title="Quarterly Plot", x = "Visit Date", y = "Median visitors") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))
    
  })
  
  output$plot2c<-renderPlotly({
    
    king2c_tmp <- king_d
    king2c_tmp$newcol <- format(as.Date(king2c_tmp$visit_date), "%Y-%m")
    king2c_tmp$visit_date <- as.Date(king2c_tmp$visit_date)
    tmp <- king2c_tmp %>%
      mutate(weekdays(visit_date)) %>%
      group_by(weekdays(visit_date), newcol) %>%
      summarize(totalvisit = median(visitors))
    
    colnames(tmp)[1] <- "DayofWeek"
    
    tmp$DayofWeek <- factor(tmp$DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                      "Friday", "Saturday", "Sunday"))
    
    fig <- tmp %>%
      plot_ly(
        x = ~DayofWeek,
        y = ~totalvisit,
        frame = ~newcol,
        type = 'bar',
        mode = 'markers',
        showlegend = F,
        texttemplate = '%{y}', textposition = 'outside'
      )
    
    X <- list(
      title = 'Day of the Week',
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    ###y axis
    
    Y <- list(
      title = "Total Visit Count",
      titlefont = list(size = 18),
      showticklabels = TRUE,
      tickfont = list(size=14),
      exponentformat = "E"
    )
    
    fig <- fig %>% layout(xaxis = X, yaxis = Y)
    fig
    fig <- fig %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      )
    fig
  })
  
  output$plot2d <- renderPlot({
    
    ggplot(king_d, aes(visitors, fill=air_genre_name)) +
      geom_histogram(alpha = 0.9, bins = 25) + xlim(0, 120) + labs("Aggregate visitor frequency", x = "Visitors", y = "Total visitor count")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))
  })
  
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)

