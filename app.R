library(ggplot2)
library(dplyr)
library(maps)
library(knitr)
library(rmarkdown)
library(shiny)

sites.frame <- read.csv("Sites.csv")
sites <- sites.frame[['Site_Name']]

zeller <- Vectorize(function(q,m,y=2020) {
  if (m %in% 1:2) {
    j <- floor((y-1)/100)
    k <- (y-1)%%100
  }
  if (m %in% 3:12) {
    j <- floor(y/100)
    k <- y%%100
  }
  n <- ((m+9)%%12)+1
  h <- ((floor(2.6*n-0.2)+q+k+floor(k/4)+floor(j/4)-2*j+6)%%7)+1
  return(h)
})

ui <- fluidPage(
  titlePanel("Weather in the UK (Jan - Nov 2020)"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Plot",
                 selectizeInput("StationsVariable", "Station(s):",
                                selected=sites[1], choices=sites, multiple=TRUE,
                                options = list(maxItems=5)
                 ),
                 selectizeInput("WeatherVariable", "Weather Variable", selected=c("WindSpeed"),
                                choices=c("Wind Speed"="wind_speed", "Air Temperature"="air_temperature",
                                          "Relative Humidity"="rltv_hum", "Visibility"="visibility"),
                                multiple=FALSE
                 ),
                 selectizeInput("AggregationVariable", "Plot Type", selected=c("month"),
                                choices=c("Hourly Data"="hour", "Daily Average"="day",
                                          "Monthly Average"="month", "Daily Maxima"="max",
                                          "Daily Minima"="min"),
                                multiple=FALSE
                 ),
                 selectizeInput("TimeTypeVariable", "Display Over...", selected=c("calendar"),
                                choices=c("Calendar Time"="calendar", "Within the Week"="inweek",
                                          "Within the Day"="inday"),
                                multiple=FALSE
                 ),
                 actionButton("button", 
                              "Update") 
        ),
        
        tabPanel("Hutton Criteria",
                 selectizeInput("HuttonStationVariable", "Station:",
                                selected=sites[1], choices=sites, multiple=TRUE,
                                options = list(maxItems=1)
                 ),
                 selectizeInput("HuttonMonthVariable", "Month:",
                                selected=c('January'), choices=c('January','February',
                                                                 'March','April','May','June','July','August',
                                                                 'September','October','November'), multiple=FALSE
                 ),
                 actionButton("button2", 
                              "Update") 
        )
      )
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Weather",
                 h2("Weather Over Time" ),
                 plotOutput("weatherPlot"),
                 tableOutput("weatherTable"),
                 downloadButton("tableDownload","Download the Table", icon=icon("download")),
                 downloadButton("tableandplotDownload", "Download the Table and Plot",
                                icon=icon("download"))
        ),
        tabPanel("Stations",
                 h2("Station Map"),
                 plotOutput("mapPlot")
        ),
        tabPanel("Hutton Criteria",
                 tableOutput("huttonTable")
        )
      )
    )
  )
)

server <- function(input, output){observeEvent(input$button, {
  
  relevant.sites <- sites.frame %>%  #filters sites df by those selected
    filter(Site_Name %in% isolate(input$StationsVariable))
  site.ids <- relevant.sites %>% #converts the site names to site ids
    pull(Site_ID)
  n <- length(site.ids) #finds out how many stations were selected
  weather.data <- data.frame(ob_time=character(), #creates an empty data frame
                             hour=integer(),
                             day=integer(),
                             month=integer(),
                             wind_speed=integer(),
                             air_temperature=integer(),
                             rltv_hum=numeric(),
                             visibility=integer(),
                             Site=integer())
  for (i in 1:n) #populates data frame with the correct site data
    weather.data <- rbind(weather.data, read.csv(paste("Site_", toString(site.ids[i]), ".csv", sep="")))
  weather.data <- weather.data %>% #add the site names
    inner_join(sites.frame,by=c('Site'='Site_ID'))
  
  output$weatherPlot <- renderPlot( {
    plot.data <- weather.data %>% #transform data for the plot
      select(-c(Latitude, Longitude)) %>%
      transform(ob_time=as.Date(ob_time))
    #remove unnecessary weather variables and adjust naming
    plot.data <- plot.data %>% 
      select(c(ob_time,hour,day,month,isolate(input$WeatherVariable),Site,Site_Name)) %>%
      rename(weather=5) 
    if (isolate(input$WeatherVariable)=="wind_speed") 
      label <- 'Wind Speed'
    else if (isolate(input$WeatherVariable)=="air_temperature")
      label <- 'Air Temperature'
    else if (isolate(input$WeatherVariable)=="rltv_hum")
      label <- 'Relative Humidity'
    else
      label <- 'Visibility'
    #determine aggregation according to user input
    if (isolate(input$AggregationVariable)=='hour') {
      yvar <- plot.data$weather
      ylabel <- 'Hourly'
    } else if (isolate(input$AggregationVariable)=='day') {
      plot.data <- plot.data %>%
        group_by(ob_time,Site_Name) %>%
        mutate(day_avg=mean(weather))
      yvar <- plot.data$day_avg
      ylabel <- 'Daily average'
    } else if (isolate(input$AggregationVariable)=='month') {
      plot.data <- plot.data %>%
        group_by(month,Site_Name) %>%
        mutate(month_avg=mean(weather))
      yvar <- plot.data$month_avg
      ylabel <- 'Monthly average'
    } else if (isolate(input$AggregationVariable)=='max') {
      plot.data <- plot.data %>%
        group_by(ob_time,Site_Name) %>%
        mutate(day_max=max(weather))
      yvar <- plot.data$day_max
      ylabel <- 'Daily maximum'
    } else {
      plot.data <- plot.data %>%
        group_by(ob_time,Site_Name) %>%
        mutate(day_min=min(weather))
      yvar <- plot.data$day_min
      ylabel <- 'Daily minimum'
    }
    #change plot type and what the x-axis represents according to user input
    if (isolate(input$TimeTypeVariable)=='calendar') { 
      plot.type <- geom_line(alpha=0.7, size=1)
      xvar <- plot.data$ob_time
      xlabel <- 'Date'
    } else if (isolate(input$TimeTypeVariable)=='inday') {
      plot.type <- geom_point(alpha=0.7, size=2)
      xvar <- plot.data$hour
      xlabel <- 'Hour of the Day'
    } else {
      plot.type <- geom_point(alpha=0.7, size=2)
      plot.data <- plot.data %>%
        mutate(day_of_week=zeller(q=day,m=month))
      xvar <- plot.data$day_of_week
      xlabel <- 'Day of the Week'
    }
    #plot the data chosen
    ggplot(data=plot.data, aes(x=xvar,y=yvar,colour=Site_Name)) + plot.type +
      xlab(xlabel) + ylab(paste(ylabel,label,sep=" ")) +
      ggtitle(paste(label," (",ylabel,")"))
  })
  
  table.data <- weather.data %>% #transform data for the table
    filter(month==11 & day %in% c(24:31)) %>%
    group_by(Site_Name,day) %>%
    summarise(wind_speed=mean(wind_speed),air_temperature=mean(air_temperature),
              rltv_hum=mean(rltv_hum),visibility=mean(visibility)) %>%
    rename(Date=day,`Site Name`=Site_Name, `Wind Speed`=wind_speed, `Air Temp`=air_temperature,
           Humidity=rltv_hum,visibility=visibility)
  output$weatherTable <- renderTable( {
    table.data
  })
  
  output$tableDownload <- downloadHandler(
    filename = "meteorological-data.csv",
    content = function(file) {
      write.csv(table.data, file, row.names=FALSE)
    }
  )
  
  
  output$tableandplotDownload <- downloadHandler( 
    filename = "meteorological-data.docx",
    content = function(file) {
      params <- list(stations=input$StationsVariable, weather=input$WeatherVariable,
                     aggregation=input$AggregationVariable, timetype=input$TimeTypeVariable)
      render("report.Rmd", output_format="word_document",
             output_file=file, params=params)
    }
  )
  
  output$mapPlot <- renderPlot( {
    map("world","UK")
    #plot all stations, and fill in the points for those selected by the user
    points(sites.frame$Longitude, sites.frame$Latitude, pch=1, col="red")
    points(relevant.sites$Longitude, relevant.sites$Latitude, pch=16, col='red')
  },height=500) 
}, ignoreNULL=FALSE) #end of content affected by first submit button
  
  observeEvent(input$button2, {
    #read in the relevant data for the Hutton variable table with same method as above
    relevant.site.hutton <- sites.frame %>%
      filter(Site_Name==isolate(input$HuttonStationVariable))
    site.id.hutton <- relevant.site.hutton %>%
      pull(Site_ID)
    weather.data.hutton <- data.frame(ob_time=character(), 
                                      hour=integer(),
                                      day=integer(),
                                      month=integer(),
                                      wind_speed=integer(),
                                      air_temperature=integer(),
                                      rltv_hum=numeric(),
                                      visibility=integer(),
                                      Site=integer())
    #no need for a for loop because only one station can be selected
    weather.data.hutton <- rbind(weather.data.hutton, read.csv(paste("Site_", 
                                                                     toString(site.id.hutton), ".csv", sep="")))
    weather.data.hutton.month <- weather.data.hutton %>%
      filter(month.name[month]==isolate(input$HuttonMonthVariable)) %>% #filter by the specific month chosen
      select(month=month, day=day,air_temperature=air_temperature,rltv_hum=rltv_hum) #select the relevant data
    n <- n_distinct(weather.data.hutton.month$day)
    hutton <- numeric(n)
    #the first 2 days of the month require data from the previous month too
    #so calculate from the 3rd onwards first
    for (i in 3:n) {
      comparison1 <- weather.data.hutton.month %>%
        filter(day==(i-2))
      comparison2 <- weather.data.hutton.month %>%
        filter(day==(i-1))
      if(min(comparison1$air_temperature)>=10 & min(comparison2$air_temperature)>=10) {
        hutton[i] <- hutton[i]+1
      }
      if (nrow(filter(comparison1, rltv_hum>=90))>=6 & nrow(filter(comparison2, rltv_hum>=90))>=6) {
        hutton[i] <- hutton[i]+1
      }
    }
    #now calculating the first two days as special cases
    #except for January which doesn't have any previous data to use
    if (isolate(input$HuttonMonthVariable)!='January') {
      weather.data.hutton.previous <- weather.data.hutton %>%
        filter(month.name[month+1]==isolate(input$HuttonMonthVariable)) %>%
        select(month=month,day=day,air_temperature=air_temperature,rltv_hum=rltv_hum)
      m <- n_distinct(weather.data.hutton.previous$day)
      comparison1prev <- weather.data.hutton.previous %>%
        filter(day==(m-1))
      comparison2prev <- weather.data.hutton.previous %>%
        filter(day==m)
      firstofmonth <- weather.data.hutton.month %>%
        filter(day==1)
      #calculating for the first day of the month
      if (min(comparison1prev$air_temperature)>=10 & min(comparison2prev$air_temperature)>=10) {
        hutton[1] <- hutton[1]+1
      }
      if (nrow(filter(comparison1prev, rltv_hum>=90))>=6 & nrow(filter(comparison2prev, rltv_hum>=90))>=6) {
        hutton[1] <- hutton[1]+1
      }
      #calculating for the second day of the month
      if (min(comparison2prev$air_temperature)>=10 & min(firstofmonth$air_temperature)>=10) {
        hutton[2] <- hutton[2]+1
      }
      if (nrow(filter(comparison2prev, rltv_hum>=90))>=6 & nrow(filter(firstofmonth, rltv_hum>=90))>=6) {
        hutton[2] <- hutton[2]+1
      }
    }
    hutton <- 2==hutton #makes it boolean and therefore human readable
    days <- unique(weather.data.hutton.month$day) #make a vector of days
    hutton.df <- data.frame(Day=days,`Is_the_Hutton_Criteria_Met`=hutton) #this data frame provides the final answer
    output$huttonTable <- renderTable( {
      hutton.df
    })
  }, ignoreNULL=FALSE) #end of content affected by second submit button
}

shinyApp(ui = ui, server = server)