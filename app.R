library(shiny)
library(shinythemes)
library(shinycustomloader)
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(lubridate)
library(DT)
library(plotly)
library(ggradar)
library(scales)

ui <- fluidPage(
    theme = shinytheme('united'),
    titlePanel('Exploring COVID-19 Cases'),
    p('Discover the number of daily confirmed, recovered and death cases around the world.'),
    hr(),
    
    fluidRow(
        column(
            width = 3,
            uiOutput('countries')
        ),
        column(
            width = 3,
            uiOutput('category')
        )
    ),
    
    tabsetPanel(
        tabPanel(
            'Cases',       
            
            h3(textOutput('cases_title')),
            withLoader(
                DTOutput('cases', width = '80%'),
                loader = 'pacman'
            )
        ),
        
        tabPanel(
            'Trendline',
            
            withLoader(
                plotlyOutput('trendline', width = '90%'),
                loader = 'pacman'
            )
        ),
        
        tabPanel(
            'Heat Map',
            
            withLoader(
                plotlyOutput('heatmap', width = '90%'),
                loader = 'pacman'
            )
        ),
        
        tabPanel(
            'Days of Week',
            
            withLoader(
                plotOutput('radar', width = '90%'),
                loader = 'pacman'
            )
        )    
    ),
    
    hr(),
    a('Data Source: Johns Hopkins University', href = 'https://github.com/CSSEGISandData/COVID-19/', target = '_blank'),
    br(),
    a('Brought to you by Ricky Soo | Free and Open Source', href = 'https://github.com/rickysoo', target = '_blank')
)

server <- function(input, output, session) {
    load_data <- reactive({
        Remote = TRUE
        
        URL_confirmed <- 'time_series_covid19_confirmed_global.csv'
        URL_recovered <- 'time_series_covid19_recovered_global.csv'
        URL_deaths <- 'time_series_covid19_deaths_global.csv'
        
        if (Remote == TRUE) {
            URL_confirmed <- paste0('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/', URL_confirmed)
            URL_recovered <- paste0('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/', URL_recovered)
            URL_deaths <- paste0('https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/', URL_deaths)
        }
        
        print('Fetching database 1')
        df_confirmed <- read.csv(URL_confirmed) %>%
            mutate(Category = 'Confirmed')
        
        print('Fetching database 2')
        df_recovered <- read.csv(URL_recovered) %>%
            mutate(Category = 'Recovered')
        
        print('Fetching database 3')
        df_deaths <- read.csv(URL_deaths) %>%
            mutate(Category = 'Deaths')
        
        print('Fetching done')
        
        df <- df_confirmed %>%
            rbind(df_recovered) %>%
            rbind(df_deaths) %>%
            rename(Country = Country.Region) %>%
            select(-Province.State, -Lat, -Long)
        # group_by(Country, Category, .drop = FALSE) %>%
        # summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        # ungroup()
        
        df[df$Country == 'US', 'Country'] <- 'United States'
        print('Data loaded')
        # write.csv(df, 'df.csv', row.names = FALSE)
        df
    })
    
    load_selected_data <- reactive({
        weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
        
        print(input$country)
        print(input$category)
        
        df <- load_data() %>%
            { if (input$country == 'All Countries') filter(., Category == input$category) else filter(., Country == input$country & Category == input$category) } %>%
            select(-Country, -Category) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE) %>%
            t() %>%
            data.frame()
        
        colnames(df) <- c('Cumulative')
        
        df <- df %>%
            mutate(
                Cases = c(df[1, 'Cumulative'], diff(df$Cumulative)),
                Date = as.Date(rownames(df), 'X%m.%d.%y'),
                Year = year(Date),
                Month = month(Date, label = TRUE),
                Day = day(Date),
                Weekday = wday(Date, label = TRUE, abbr = FALSE)
            ) %>%
            mutate(
                Weekday = factor(Weekday, levels = weekdays)
            )
        
        # write.csv(df, 'selected_df.csv', row.names = FALSE)
        
        print('Selected data loaded')
        df[, c('Date', 'Cases', 'Year', 'Month', 'Day', 'Weekday')]
    })
    
    load_cases <- reactive({
        df <- load_data() %>%
            { if (input$country == 'All Countries') filter(., TRUE) else filter(., Country == input$country) } %>%
            select(-Country) %>%
            group_by(Category, .drop = FALSE) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE) %>%
            select(-Category) %>%
            t() %>%
            data.frame()
        
        colnames(df) <- c('CumConfirmed', 'CumDeaths', 'CumRecovered')
        
        df <- df %>%
            mutate(
                Date = as.Date(rownames(df), 'X%m.%d.%y'),
                Confirmed = c(df[1, 'CumConfirmed'], diff(df$CumConfirmed)),
                Recovered = c(df[1, 'CumRecovered'], diff(df$CumRecovered)),
                Deaths = c(df[1, 'CumDeaths'], diff(df$CumDeaths))
            )
        
        print('Cases loaded')
        df[, c('Date', 'Confirmed', 'Recovered', 'Deaths')]
    })
    
    output$countries <- renderUI({
        items <- sort(unique(load_data()[['Country']]))
        
        selectInput(
            inputId = 'country',
            label = NULL,
            choices = c('All Countries' = 'All Countries', items),
            selected = 'All Countries'
        )
    })
    
    output$category <- renderUI({
        items <- unique(load_data()[['Category']])
        
        selectInput(
            inputId = 'category',
            label = NULL,
            choices = items,
            selected = 'Confirmed'
        )
    })
    
    output$cases_title <- renderText({
        if (is.null(input$country) | is.null(input$category)) {
            return(NULL)
        }
        
        paste0(input$country, ' - Daily Cases')
    })  
    
    output$cases <- renderDT(
        {
            if (is.null(input$country)) {
                return(NULL)
            }
            
            datatable(
                load_cases(),
                rownames = FALSE,
                filter = 'top',
                class = 'cell-border compact stripe',
                extensions = c('Responsive'),
                
                options = list(
                    scrollX = TRUE,
                    pageLength = 30
                )
            ) %>%
                formatDate(
                    columns = 1,
                    method = 'toLocaleDateString'
                )
        }
    )
    
    output$trendline <- renderPlotly({
        if (is.null(input$country) | is.null(input$category)) {
            return(NULL)
        }
        
        line_color <- switch (
            input$category,
            'Confirmed' = 'black',
            'Recovered' = 'blue',
            'Deaths' = 'red'
        )   
        
        fig <- ggplot(load_selected_data(), aes(Date, Cases)) +
            geom_line(color = line_color) +
            geom_smooth(method = 'loess', formula = y ~ x, se = FALSE, linetype = 'dotted', color = 'grey', alpha = 0.2, weight = 1) +
            theme_minimal(base_size = 8) +
            labs(x =  'Date', y = 'Cases') +
            theme(plot.title = element_text(size = 14)) +
            theme(axis.text.y = element_text(size = 6)) +
            theme(plot.title = element_text(hjust = 0)) +
            theme(axis.ticks = element_blank()) +
            theme(axis.text = element_text(size = 7)) +
            theme(legend.position = 'none') +
            removeGrid()
        
        fig <- ggplotly(fig) %>%
            layout(
                margin = 20,
                title = paste0('Daily ', input$category, ' Cases in ', input$country),
                showlegend = FALSE
            )
        fig
    })  
    
    output$heatmap <- renderPlotly({
        if (is.null(input$country) | is.null(input$category)) {
            return(NULL)
        }
        
        low_color <- switch (
            input$category,
            'Confirmed' = 'blue',
            'Recovered' = 'yellow',
            'Deaths' = 'blue'
        )   
        
        high_color <- switch (
            input$category,
            'Confirmed' = 'black',
            'Recovered' = 'green',
            'Deaths' = 'red'
        )   
        
        fig <- ggplot(load_selected_data(), aes(Month, Day, fill = Cases)) +
            scale_fill_gradient(low = low_color, high = high_color) +
            geom_tile(color = 'white', size = 0.1) +
            facet_grid(cols = vars(Year)) +
            scale_y_continuous(trans = 'reverse') +
            theme_minimal(base_size = 8) +
            labs(title = paste0('Daily ', input$category, ' Cases in ', input$country), x =  'Month', y = 'Day') +
            theme(legend.position = 'right') +
            theme(plot.title = element_text(size = 14)) +
            theme(axis.text.y = element_text(size = 6)) +
            theme(strip.background = element_rect(colour = 'white')) +
            theme(plot.title = element_text(hjust = 0)) +
            theme(axis.ticks = element_blank()) +
            theme(axis.text = element_text(size = 7)) +
            theme(legend.title = element_text(size = 8)) +
            theme(legend.text = element_text(size = 6)) +
            removeGrid()
        
        fig <- ggplotly(fig) %>%
            layout(
                margin = 20
            )
        fig
    })  
    
    output$radar <- renderPlot({
        weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
        
        df <- load_cases() %>%
            mutate(
                Weekday = wday(Date, label = TRUE, abbr = FALSE)
            ) %>%
            mutate(
                Weekday = factor(Weekday, levels = weekdays)
            ) %>%
            group_by(Weekday) %>%
            summarise(
                Confirmed = sum(Confirmed),
                Recovered = sum(Recovered),
                Deaths = sum(Deaths)
            ) %>%
            mutate(
                Confirmed = rescale(Confirmed, from = c(0, max(Confirmed))),
                Recovered = rescale(Recovered, from = c(0, max(Recovered))),
                Deaths = rescale(Deaths, from = c(0, max(Deaths)))
            ) %>%
            column_to_rownames('Weekday') %>%
            t() %>%
            data.frame() %>%
            rownames_to_column('Category')

        fig <- ggradar(
            df,
            values.radar = c(''),

            grid.min = 0,
            grid.mid = 0.5,
            grid.max = 1,

            group.line.width = 1,
            group.point.size = 3,
            group.colours = c('black', 'red', 'green'),
            
            background.circle.colour = 'white',
            gridline.min.colour = 'grey',
            
            plot.title = paste0('Daily Cases in ', input$country, ' Over a Week'),
            legend.title = 'Category',
            legend.position = 'right'
        )
        
        fig
    })
}

shinyApp(ui, server)