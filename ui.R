shinyUI(fluidPage(
    titlePanel(h1('Top Year-To-Date S&P 500 Index Members',align='center')),
    sidebarLayout(
        sidebarPanel(
            h2('User Input Preferences',align='center'),
            br(),
            img(src='updown.jpg',height=300,width=300,filetype='image/jpeg',align='center'),
            br(),
            h3('Number of Members',align='center'),
            br(),
            sliderInput(inputId = 'members',label = p('Set number of members.'),value=10,min = 1,max=50,step = 1),
            br(),
            h3('Performance Metric',align='center'),
            br(),
            radioButtons(inputId = 'metric',label = p('Choose Performance Metric.'),choices = c('Cumulative Rate of Return','Annualized Standard Deviation','Return2Volatility Ratio')),
            br()
            ),
        mainPanel(
            tabsetPanel(
                tabPanel(h3('Instructions'),includeHTML('instructions.html')),
                tabPanel(h3('Plot'),textOutput(outputId = 'textPlot'),plotOutput('plot',width = 1100,height = 1000)),
                tabPanel(h3('Performance List'),textOutput(outputId = 'performanceList'),tableOutput(outputId = 'performanceTable'))
            )
        )
    )
))
