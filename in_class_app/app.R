

# My Shiny App ---------------------------------------------------------------




# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library (readr)
library(scales)
library(ggthemes)
library(shinythemes)

# Data --------------------------------------------------------------------

urlfile="https://raw.githubusercontent.com/jeffreyxparker/stat495rFall2020/master/Week%2014%20-%20RShiny/orders.csv"

orders <-read_csv(url(urlfile))

orders <- orders %>% 
    mutate(
        created_at = mdy(created_at),
        month = month(created_at, label = TRUE),
        year = year(created_at),
        day = day(created_at)
    )

orders_cum <- orders %>%
    filter(year == 2020) %>%
    arrange(month) %>% 
    mutate(
        cum_rev = cumsum(total_price)
    )

orders_sub <- orders_cum %>%
    arrange(created_at) %>% 
    group_by(created_at) %>% 
    summarize(
        n_subs = n_distinct(id)
    ) %>% 
    mutate(
        cum_subs = cumsum(n_subs)
    )

orders_clean <- inner_join(orders_cum, orders_sub)
    

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    # Application title
    titlePanel("Are We Reaching Our Monthly Goals?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # toggle if cummulative or just current month
            # radioButtons(
            #     "timeframe",
            #     "Select Timeframe",
            #     choices = c("Yearly", "Monthly"),
            #     selected = "Monthly"
            # ),
            selectInput(
                "month",
                "Select Month",
                choices = levels(unique(orders$month)),
                selected = "Dec"
                # multiple = TRUE
            ),
            
            numericInput(
                "subgoal",
                "Enter Monthly Subscription Goal",
                value = 500,
                min = 0
            ),
            
            numericInput(
                "revgoal",
                "Enter Monthly Revenue Goal",
                value = 20000,
                min = 0
            )
        ),

        # Show a plot of the generated distribution Cumulative
        mainPanel(
            
            ## Add text
            
            # Plot the Month to Month Plots
            # paste("Cumulative Subscribers"),
            plotOutput("submonPlot"),
            
            # paste("Cumulative Revenue"),
            plotOutput("revmonPlot")
            
            # conditionalPanel(
            #     condition = "input$timeframe == 'Yearly'",
            #     plotOutput("subcumPlot")
            #     
            # )
            
            # plotOutput("revcumPlot")
           
        )
    )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Subscribers Cumulative Plot
    output$subcumPlot <- renderPlot({
        # 
        x <- orders %>%
            filter(year == 2020) %>% 
            # filter(month == "Mar") %>% 
            group_by(month) %>% 
            # filter(month == "Mar") %>% 
            summarize(
               n_subs = n_distinct(id)
            )
        # draw the histogram with the specified number of bins
        ggplot(x) +
            geom_col(aes(y = cumsum(n_subs), x = month)) +
            geom_line(aes(y = n_subs, x = month), group = 1)
        
        ## Add a highlighted Month function to show how many subs in that month
        
        ## Add pace line
    })
    
    # Subscribers Cumulative Plot
    output$revcumPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        y <- orders %>%
            filter(year == 2020) %>% 
            group_by(month) %>% 
            # filter(month == "Mar") %>% 
            summarize(
                n_rev = sum(total_price)
            )
        # draw the histogram with the specified number of bins
        ggplot(y) +
            geom_col(aes(y = cumsum(n_rev), x = month)) +
            geom_line(aes(y = n_rev, x = month), group = 1)
            # annotate("segment", x = "Jan", xend = "Dec", y = 2, yend = input$revgoal, arrow = arrow(type = "open"), color = "grey60")
    })
    
    # Subscribers Monthly Plot
    output$submonPlot <- renderPlot({
        # 
        z <- orders %>%
            filter(year == 2020) %>% 
            filter(month == input$month) %>%
            group_by(month, day) %>% 
            summarize(
                n_subs = n_distinct(id)
            )
        # draw the histogram with the specified number of bins
       z %>% 
           ggplot() +
           geom_col(aes(y = cumsum(n_subs), x = day), fill = "#1B9E77") +
           geom_line(aes(y = n_subs, x = day), color = "grey80", group = 1) +
           annotate("segment", x = 1, xend = 31, y = 2, yend = input$subgoal, color = "#A5771D") +
           annotate("text", x = 25, y = input$subgoal, label = paste("Goal:", input$subgoal), color = "#A5771D", vjust = 3) +
           annotate("text", x = 0, y = 200, label = "Actual Subscribers", color = "grey60", angle = 90) +
           annotate("text", x = 0, y = 500, label = "Cumulative Subscribers", color = "#1B9E77", angle = 90, hjust = 1) +
           labs(
               title = paste("Cumulative Subscribers for", input$month, "2020"),
               y = "",
               x = "Day"
           ) +
           theme_tufte() +
           theme(
               axis.ticks.y = element_blank(),
               axis.line.x = element_line(),
               text = element_text(family = "Helvetica Neue"),
               plot.title = element_text(size = 20)
           ) +
           scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600), expand = c(0,0))
        
        ## Add a highlighted Month function to show how many subs in that month
        
        ## Add pace line
    })
    
    # Revenue Monthly Plot
    output$revmonPlot <- renderPlot({
        # 
        z <- orders %>%
            filter(year == 2020) %>% 
            filter(month == input$month) %>%
            group_by(month, day) %>% 
            summarize(
                n_rev = sum(total_price)
            )
        # z <- orders %>%
        #     filter(year == 2020) %>% 
        #     filter(month == c("Mar", "Apr")) %>%
        #     group_by(month, day) %>% 
        #     summarize(
        #         n_rev = sum(total_price),
        #         month_day = str_c(month, day, sep = " ")
        #     )
        # draw the histogram with the specified number of bins
        z %>% 
            ggplot() +
            geom_col(aes(y = cumsum(n_rev), x = day), fill = "#1B9E77") +
            geom_line(aes(y = n_rev, x = day), color = "grey80", group = 1) +
            annotate("segment", x = 1, xend = 31, y = 2, yend = input$revgoal, color = "#A5771D") +
            annotate("text", x = 25, y = input$revgoal, label = paste("Goal: $", input$revgoal), color = "#A5771D", vjust = 3) +
            annotate("text", x = 0, y = 7500, label = "Actual Revenue", color = "grey60", angle = 90) +
            annotate("text", x = 0, y = 20000, label = "Cumulative Revenue", color = "#1B9E77", angle = 90, hjust = 1) +
            labs(
                title = paste("Cumulative Revenue for", input$month, "2020"),
                y = "",
                x = "Day"
            ) +
            theme_tufte() +
            theme(
                axis.ticks.y = element_blank(),
                axis.line.x = element_line(),
                text = element_text(family = "Helvetica Neue"),
                plot.title = element_text(size = 20)
            ) +
            scale_y_continuous(label = label_number(prefix = "$", suffix = "K", scale = 1/1e3, accuracy = 1), breaks = c(5000, 10000, 15000, 20000), expand = c(0,0))
        
        ## Add a highlighted Month function to show how many subs in that month
        
        ## Add pace line
    })
}


# Run App -----------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
