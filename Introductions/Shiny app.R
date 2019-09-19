library(shiny)

# Define server logic required to generate and plot a random distribution
#
# Adapted from original code by Pierre Chretien
# References
# Rstudio / shiny-examples (2016, Aug 27).  060-retirement-simulation.  Retrieved on April 14, 2018 from https://github.com/rstudio/shiny-examples/tree/master/060-retirement-simulation
# 
#
server <- function(input, output) {
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  getNav <- reactive({
    #-------------------------------------
    # Inputs
    #-------------------------------------
    
    # Initial capital
    start.capital = input$start.capital
    
    # Investment
    annual.mean.return = input$annual.mean.return / 100
    annual.ret.std.dev = input$annual.ret.std.dev / 100
    
    # Inflation
    annual.inflation = input$annual.inflation / 100
    annual.inf.std.dev = input$annual.inf.std.dev / 100
    
    # Deposits
    monthly.deposits = -input$monthly.deposits
    
    # Number of years before retirement
    n.obs = input$n.obs
    
    # Number of scenarios
    n.sim = input$n.sim
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    # number of months to simulate
    n.obs = 12 * n.obs
    
    
    # monthly Investment and Inflation assumptions
    monthly.mean.return = annual.mean.return / 12
    monthly.ret.std.dev = annual.ret.std.dev / sqrt(12)
    
    monthly.inflation = annual.inflation / 12
    monthly.inf.std.dev = annual.inf.std.dev / sqrt(12)
    
    
    # simulate Returns
    monthly.invest.returns = matrix(0, n.obs, n.sim)
    monthly.inflation.returns = matrix(0, n.obs, n.sim)
    
    monthly.invest.returns[] = rnorm(n.obs * n.sim, mean = monthly.mean.return, sd = monthly.ret.std.dev)
    monthly.inflation.returns[] = rnorm(n.obs * n.sim, mean = monthly.inflation, sd = monthly.inf.std.dev)
    
    # simulate Deposits
    nav = matrix(start.capital, n.obs + 1, n.sim)
    for (i in 1:n.obs) {
      nav[i + 1, ] = nav[i, ] * (1 + monthly.invest.returns[i, ] - monthly.inflation.returns[i, ]) + monthly.deposits
    }
    
    # once nav is below 0 => run out of money
    nav[ nav < 0 ] = NA
    
    # convert to millions
    nav = nav / 1000000
    
    return(nav)
  })
  
  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$distPlot <- renderPlot({
    nav = getNav()
    
    layout(matrix(c(1,2,1,3),2,2))
    
    # plot all scenarios
    matplot(nav, type = 'l', las = 1, xlab = 'Months', ylab = 'Millions',
            main = 'Projected Value of Portfolio over Time (sample scenarios)')
    
    # TODO: replace with a plot stack chart comparing average ratio of monthly deposit vs. initial capital

    # plot % of scenarios that are still paying
    p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)
    
    plot(100 * p.alive, las = 1, xlab = 'Months', ylab = 'Percentage Paying',
         main = 'Percentage of Paying Scenarios', ylim=c(0,100))
    grid()
    
    
    last.period = nrow(nav)    

    # plot distribution of final wealth
    final.nav = nav[last.period, ]
    final.nav = final.nav[!is.na(final.nav)]
    
    if(length(final.nav) ==  0) return()
    
    plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
         main = paste('Distribution of Final Capital,', 100 * p.alive[last.period], '% are still paying'))
    grid()
    
    
    
  })
  
}


# Define UI for application that plots random distributions
ui <- function(input,output){
pageWithSidebar(
  
  # Application title
  headerPanel("Retirement Preparation Simulation"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("n.obs",
                "Number of years in Retirement:",
                min = 0,
                max = 40,
                value = 20),
    
    sliderInput("start.capital",
                "Total savings at Retirement:",
                min = 0,
                max = 10000000,
                value = 1200000,
                step = 10000,
                pre = "$",
                sep = ","),

    sliderInput("monthly.deposits",
                "Monthly Retirement Withdrawals:",
                min = 0,
                max = 20000,
                value = 3000,
                step = 500,
                pre = "$",
                sep = ","),
        
    sliderInput("n.sim",
                "Number of scenarios:",
                min = 0,
                max = 100,
                value = 10),
    
    sliderInput("annual.mean.return",
                "Annual investment return (in %):",
                min = 0.0,
                max = 30.0,
                value = 5.0,
                step = 0.5),
    
    sliderInput("annual.ret.std.dev",
                "Annual investment volatility (in %):",
                min = 0.0,
                max = 25.0,
                value = 7.0,
                step = 0.1),
    
    sliderInput("annual.inflation",
                "Annual inflation (in %):",
                min = 0,
                max = 20,
                value = 2.5,
                step = 0.1),
    
    sliderInput("annual.inf.std.dev",
                "Annual inflation volatility. (in %):",
                min = 0.0,
                max = 5.0,
                value = 1.5,
                step = 0.05)

  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot", height = "700px")
  )
)
}
# Run the application 
shinyApp(ui = ui, server = server)

