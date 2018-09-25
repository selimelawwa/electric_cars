
library(shiny)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)

ecv <- read.csv("data/Total ECV by Quarter.csv")
ecv[ecv==""] <- NA
data <- ecv %>% gather(`Q2.2018`, `Q1.2018`,`Q4.2017`,`Q3.2017`,`Q2.2017`,`Q1.2017`,
                       `Q4.2016`,`Q3.2016`,`Q2.2016`,`Q1.2016`,
                       `Q4.2015`,`Q3.2015`,`Q2.2015`,`Q1.2015`,key = "quarter_year", value = "number_of_cars")
data <- data %>% separate(quarter_year, c("quarter", "year")) %>% arrange(desc(year),desc(quarter))
data$year <- as.integer(data$year)
data$yearqtr <- as.yearqtr(paste(data$year," ",data$quarter) )%>% format(tt,format = "%Y-Q%q")
country_data_per_year <- data%>% group_by(Country,year) %>% summarise(total = sum(number_of_cars)) %>% arrange(Country,year)

ui <- fluidPage(
   
   # Application title
   titlePanel("Electric Car in EU "),
   
   tabsetPanel(
     tabPanel("Plot", br(), br(), fluidRow(
       column(3,
              selectInput("filter", "Filter by market size",
                          c("Large", "Medium","Small","All"),
                          selected = "Large"),
              selectInput("year","Select Year",c(unique(data[["year"]])),selected = "2017")),
       column(9, plotlyOutput("plot1"))) 
     ),
     tabPanel("Bar chart comparing years", br(), br(), fluidRow(
       column(3,
              selectInput("filter2", "Filter by market size",
                          c("Large", "Medium","Small","All"),
                          selected = "Large"),
              checkboxGroupInput("check_year", 
                                 h3("Filter by year"), 
                                 c("2015","2016","2017"),selected = "2017")),
       column(9, plotlyOutput("plot2"))) 
     )
     
     )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  data_plot1 <- reactive({
    if (input$filter == "All"){
      subset(data,year== input$year) %>% group_by(Country) %>% 
        summarise(total = sum(number_of_cars))
    }else if (input$filter == "Large"){
      subset(data,year== input$year) %>% group_by(Country) %>% 
        summarise(total = sum(number_of_cars)) %>%  top_n(n = 5, wt = total) 
    }else if (input$filter == "Medium"){
      subset(data,year== input$year) %>% group_by(Country) %>% 
        summarise(total = sum(number_of_cars)) %>%  top_n(n = -20, wt = total) %>%  top_n(n = 8, wt = total)
    }else if (input$filter == "Small"){
      subset(data,year== input$year) %>% group_by(Country) %>% 
        summarise(total = sum(number_of_cars)) %>%  top_n(n = -12, wt = total) 
    }
    
  })
  
  output$plot1 <- renderPlotly({
    dp1 <- data_plot1()
    plot <- ggplot(data = dp1,aes(x=reorder(Country, -total),y=total))+
        geom_col(fill="dodgerblue4")+
        theme_classic()+
        theme(axis.title.x=element_blank())+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Cars")
    if (input$year == "2018"){
      plot + ggtitle(paste("Number of newly registered Electric cars in ",input$year, " (first 2 quarters)"))
    }else{
      plot + ggtitle(paste("Number of newly registered Electric cars in ",input$year))
    }
    
  })
  
  data_plot2 <- reactive({
    p <- data
    
    if (length(input$check_year)==1){
      p <- subset(data, year==input$check_year[1])
    } else if (length(input$check_year)==2){
      p <- subset(data, year==input$check_year[1] | year==input$check_year[2])
    } else if (length(input$check_year)==3){
      p <- subset(data, year==input$check_year[1] | year==input$check_year[2] | year==input$check_year[3])
    } else{
      p <- subset(data, year=="2015" | year=="2016" | year=="2017")
    }
    
    if (input$filter2 == "All"){
      p %>% group_by(year,Country) %>%  summarise(total = sum(number_of_cars)) 
    }else if (input$filter2 == "Large"){
      c <- p %>% group_by(Country) %>% summarise(total = sum(number_of_cars)) %>%  top_n(n = 5, wt = total) 
      c <- c$Country
      filter(p, Country %in% c) %>% group_by(Country,year) %>% summarise(total = sum(number_of_cars))
    }else if (input$filter2 == "Medium"){
      c <- p %>% group_by(Country) %>% summarise(total = sum(number_of_cars)) %>%  top_n(n = -20, wt = total) %>%  top_n(n = 9, wt = total)
      c <- c$Country
      filter(p, Country %in% c) %>% group_by(Country,year) %>% summarise(total = sum(number_of_cars))
    }else if (input$filter2 == "Small"){
      c <- p %>% group_by(Country) %>% summarise(total = sum(number_of_cars)) %>%  top_n(n = -11, wt = total) 
      c <- c$Country
      filter(p, Country %in% c) %>% group_by(Country,year) %>% summarise(total = sum(number_of_cars))
    }
  })
  
  output$plot2 <- renderPlotly({
    dp2 <- data_plot2()
    ggplot(data = dp2,aes(x=reorder(Country, -total),y=total,fill=as.factor(year)))+
      geom_bar(stat="identity",position = position_dodge2(preserve = "single"))+
      scale_fill_manual("Year",values = c("2015" = "dodgerblue1", "2016" = "dodgerblue4", "2017" = "navyblue"))+
      theme_classic()+
      theme(axis.title.x=element_blank())+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Cars") +
      ggtitle(paste("Number of newly registered Electric cars in ",input$year))
    
    
  })
  
  
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

