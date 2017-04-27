library(ggplot2)
library(ggvis)
library(shiny)
library(magrittr)
library(GGally)

setwd('~/Desktop/MSAN/MSAN622/Homework/HW3')

fb <- read.delim('Facebook_metrics/dataset_Facebook.csv', sep = ';')

colnames(fb) <- c('Page.likes', 'Type', 'Category', 'Month', 'Weekday', 'Hour',
                  'Paid', 'Reach', 'Impressions', 'Engaged', 'Consumers',
                  'Consumptions', 'Impressions.by.liked', 'Reach.by.liked',
                  'Engaged.by.liked', 'Comments', 'Likes', 'Shares', 'Interactions')

fb_complete <- fb[complete.cases(fb),]

fb_agg <- aggregate(fb, by = list(fb$Month, fb$Weekday, fb$Hour), mean, na.rm = TRUE)

fb_agg$Weekday.by.Month <- paste(fb_agg$Month, fb_agg$Weekday, sep = '-')

fb_agg$Hour <- factor(fb_agg$Hour, levels = 1:24)

fb_agg$Month <- factor(fb_agg$Month, levels = 1:12, labels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

fb_complete$Month <- factor(fb_complete$Month, levels = 1:12, labels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

fb_complete$Weekday <- factor(fb_complete$Weekday, levels = 1:7, labels = c('Sun', 'Mon', 'Tues', 'Weds', 'Thurs', 'Fri', 'Sat'))

fb_complete$Weekend <- factor(ifelse(fb_complete$Weekday %in% c('Sat', 'Sun'), 'Weekend', 'Weekday'))

month_week_combos <- vector()

theme_fb <- theme(
  axis.line = element_blank(), 
  axis.ticks = element_line(size = 0.1),
  panel.background = element_rect(fill = 'azure2'))

for (i in 1:12){
  for (j in 1:5){
    ij <- paste(as.character(i), as.character(j), sep = '-')
    month_week_combos <- c(month_week_combos, ij)
  }
}

fb_agg$Weekday.by.Month <- factor(fb_agg$Weekday.by.Month, levels = month_week_combos)

palette_fb <- c("#758ab6","#111a2d")

ui <- fluidPage(
  headerPanel("Facebook post engagement"),
  mainPanel(
    tabsetPanel(
      tabPanel("Heatmap",
               sidebarPanel(
                 selectInput("heat_metric", "Metric", c("Impressions", "Comments", "Likes", "Shares")),
                 selectInput("heat_month", "Month", levels(fb_agg$Month))
               ),
               mainPanel(
                 plotOutput("heat",
                            hover = "heat_hover"),
                 verbatimTextOutput("heat_info")
               )
               ),
      tabPanel("Scatterplots",
               sidebarPanel(
                 selectInput("scatter_month", "Month", levels(fb_complete$Month)),
                 checkboxGroupInput("scatter_dow", "Weekday",c('Sun', 'Mon', 'Tues', 'Weds', 'Thurs', 'Fri', 'Sat'), selected = c('Sun', 'Mon', 'Tues', 'Weds', 'Thurs', 'Fri', 'Sat'))
               ),
               mainPanel(
               plotOutput("scatter",
                          hover = "scatter_hover"),
               verbatimTextOutput("scatter_info")
               )
               ),
      tabPanel("Parallel Coordinates Plot",
               sidebarPanel(
                 selectInput("parallel_month", "Month", levels(fb_complete$Month))
               ),
               mainPanel(
               plotOutput("parallel",
                          hover = "parallel_hover"),
               verbatimTextOutput("parallel_info")
               )
               )
    )
  )
)

server <- function(input, output) {
  heat_selected <- reactive({
    fb_agg[fb_agg$Month == input$heat_month,]
    })
  output$heat <- renderPlot({
    ggplot(heat_selected(), aes(Weekday, Hour, fill = fb_agg[fb_agg$Month == input$heat_month,input$heat_metric])) +  
      geom_tile() + theme_fb + scale_fill_gradient(low = 'white', high = '#3b5998') + labs(fill = 'Engagement', x = 'Day of Week', y = 'Hour Posted') +
      scale_x_discrete(limits = 1:7, labels = c('Sun', 'Mon', 'Tues', 'Weds', 'Thurs', 'Fri', 'Sat'), position = 'top') + scale_y_discrete(limits = 1:24, position = 'right')
  })
  scatter_selected <- reactive({
    fb_complete[(fb_complete$Month == input$scatter_month) & (fb_complete$Weekday %in% input$scatter_dow), c('Impressions', 'Comments', 'Likes', 'Shares')]
  })
  output$scatter <- renderPlot({
    ggpairs(scatter_selected()) + theme_bw()
  })
  parallel_selected <- reactive({
    fb_complete[fb_complete$Month == input$parallel_month,]
  })
  output$parallel <- renderPlot({
    ggparcoord(data = parallel_selected(), columns = 16:18, mapping = ggplot2::aes(color = factor(Weekend))) + theme_fb +
      labs(x = 'Variable', y = 'Value', color = 'Day of Week') + scale_color_manual(values = palette_fb, breaks = c(1, 2), labels = c('Weekday', 'Weekend'))
  })
}

shinyApp(ui = ui, server = server)
