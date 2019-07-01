## Only run examples in interactive R sessions
library(shiny)
library(shinythemes)
library(ERSA)
library(ggplot2)
library(readxl)
library(sjPlot)
library(sjmisc)
library(usmap)


GDP_data <- read_excel("../GDP/State Leading Index Correlation to SPY and GDP.xls", sheet = "State Economy Leading Indexes", col_types = c("date", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
spy <- read_excel("../GDP/State Leading Index Correlation to SPY and GDP.xls", sheet = "SPY Total Return")
usgdp <- read_excel("../GDP/State Leading Index Correlation to SPY and GDP.xls", sheet = "US GDP")



# Define UI for application
ui <- tagList(
    #Pick a theme for the app. Recommended theme is darkly.
    shinythemes::themeSelector(),
    fluidPage(theme = shinytheme("darkly"),
              
              headerPanel(title = 'Data Analysis using the GDP dataset'),
              sidebarPanel(
                  selectInput('Gvndate', 'Date for a Map Plot of State Economy Index Data', choices = GDP_data$Date),
                  hr(),
                  selectInput('DepVar', 'Dependent Variable for a regression on State Economy Index data', choices = list('United States' = 'US', 'Alabama'= 'AL', 'Alaska'= 'AK', 'Arizona'= 'AZ', 'Arkansas'= 'AR', 'California'= 'CA', 'Colorado'= 'CO', 'Connecticut'= 'CT', 'Delaware'= 'DE', 'Florida'= 'FL', 'Georgia'= 'GA', 'Hawaii'= 'HI', 'Idaho'= 'ID', 'Illinois'= 'IL', 'Indiana'= 'IN', 'Iowa'= 'IA', 'Kansas'= 'KS', 'Kentucky'= 'KY', 'Louisiana'= 'LA', 'Maine'= 'ME', 'Maryland'= 'MD', 'Massachusetts'= 'MA', 'Michigan'= 'MI', 'Minnesota'= 'MN', 'Mississippi'= 'MS', 'Missouri'= 'MO', 'Montana'= 'MT', 'Nebraska'= 'NE', 'Nevada'= 'NV', 'New Hampshire'= 'NH', 'New Jersey'= 'NJ', 'New Mexico'= 'NM', 'New York'= 'NY', 'North Carolina'= 'NC', 'North Dakota'= 'ND', 'Ohio'= 'OH', 'Oklahoma'= 'OK', 'Oregon'= 'OR', 'Pennsylvania'= 'PA', 'Rhode Island'= 'RI', 'South Carolina'= 'SC', 'South Dakota'= 'SD', 'Tennessee'= 'TN', 'Texas'= 'TX', 'Utah'= 'UT', 'Vermont'= 'VT', 'Virginia'= 'VA', 'Washington'= 'WA', 'West Virginia'= 'WV', 'Wisconsin'= 'WI', 'Wyoming'= 'WY')),
                  selectInput("IndVar", "Independent variables for a regression analysis on State Economy Index Data", selected = c("AL", "AR"),  choices = list('United States' = 'US', 'Alabama'= 'AL', 'Alaska'= 'AK', 'Arizona'= 'AZ', 'Arkansas'= 'AR', 'California'= 'CA', 'Colorado'= 'CO', 'Connecticut'= 'CT', 'Delaware'= 'DE', 'Florida'= 'FL', 'Georgia'= 'GA', 'Hawaii'= 'HI', 'Idaho'= 'ID', 'Illinois'= 'IL', 'Indiana'= 'IN', 'Iowa'= 'IA', 'Kansas'= 'KS', 'Kentucky'= 'KY', 'Louisiana'= 'LA', 'Maine'= 'ME', 'Maryland'= 'MD', 'Massachusetts'= 'MA', 'Michigan'= 'MI', 'Minnesota'= 'MN', 'Mississippi'= 'MS', 'Missouri'= 'MO', 'Montana'= 'MT', 'Nebraska'= 'NE', 'Nevada'= 'NV', 'New Hampshire'= 'NH', 'New Jersey'= 'NJ', 'New Mexico'= 'NM', 'New York'= 'NY', 'North Carolina'= 'NC', 'North Dakota'= 'ND', 'Ohio'= 'OH', 'Oklahoma'= 'OK', 'Oregon'= 'OR', 'Pennsylvania'= 'PA', 'Rhode Island'= 'RI', 'South Carolina'= 'SC', 'South Dakota'= 'SD', 'Tennessee'= 'TN', 'Texas'= 'TX', 'Utah'= 'UT', 'Vermont'= 'VT', 'Virginia'= 'VA', 'Washington'= 'WA', 'West Virginia'= 'WV', 'Wisconsin'= 'WI', 'Wyoming'= 'WY'), multiple = T),
                  hr(),
                  selectInput('Cordep', 'Dependent Variable for the correlation analysis', selected = "AL",  choices = list('United States' = 'US', 'Alabama'= 'AL', 'Alaska'= 'AK', 'Arizona'= 'AZ', 'Arkansas'= 'AR', 'California'= 'CA', 'Colorado'= 'CO', 'Connecticut'= 'CT', 'Delaware'= 'DE', 'Florida'= 'FL', 'Georgia'= 'GA', 'Hawaii'= 'HI', 'Idaho'= 'ID', 'Illinois'= 'IL', 'Indiana'= 'IN', 'Iowa'= 'IA', 'Kansas'= 'KS', 'Kentucky'= 'KY', 'Louisiana'= 'LA', 'Maine'= 'ME', 'Maryland'= 'MD', 'Massachusetts'= 'MA', 'Michigan'= 'MI', 'Minnesota'= 'MN', 'Mississippi'= 'MS', 'Missouri'= 'MO', 'Montana'= 'MT', 'Nebraska'= 'NE', 'Nevada'= 'NV', 'New Hampshire'= 'NH', 'New Jersey'= 'NJ', 'New Mexico'= 'NM', 'New York'= 'NY', 'North Carolina'= 'NC', 'North Dakota'= 'ND', 'Ohio'= 'OH', 'Oklahoma'= 'OK', 'Oregon'= 'OR', 'Pennsylvania'= 'PA', 'Rhode Island'= 'RI', 'South Carolina'= 'SC', 'South Dakota'= 'SD', 'Tennessee'= 'TN', 'Texas'= 'TX', 'Utah'= 'UT', 'Vermont'= 'VT', 'Virginia'= 'VA', 'Washington'= 'WA', 'West Virginia'= 'WV', 'Wisconsin'= 'WI', 'Wyoming'= 'WY')),
                  selectInput("CorInd", "Independent variable for a correlation analysis", selected = "AR",  choices = list('Spy Data Close Value'= "Close", 'Spy Data Volume'= 'Volume', "US GDP Dataset GDP" = "GDP", 'United States' = 'US', 'Alabama'= 'AL', 'Alaska'= 'AK', 'Arizona'= 'AZ', 'Arkansas'= 'AR', 'California'= 'CA', 'Colorado'= 'CO', 'Connecticut'= 'CT', 'Delaware'= 'DE', 'Florida'= 'FL', 'Georgia'= 'GA', 'Hawaii'= 'HI', 'Idaho'= 'ID', 'Illinois'= 'IL', 'Indiana'= 'IN', 'Iowa'= 'IA', 'Kansas'= 'KS', 'Kentucky'= 'KY', 'Louisiana'= 'LA', 'Maine'= 'ME', 'Maryland'= 'MD', 'Massachusetts'= 'MA', 'Michigan'= 'MI', 'Minnesota'= 'MN', 'Mississippi'= 'MS', 'Missouri'= 'MO', 'Montana'= 'MT', 'Nebraska'= 'NE', 'Nevada'= 'NV', 'New Hampshire'= 'NH', 'New Jersey'= 'NJ', 'New Mexico'= 'NM', 'New York'= 'NY', 'North Carolina'= 'NC', 'North Dakota'= 'ND', 'Ohio'= 'OH', 'Oklahoma'= 'OK', 'Oregon'= 'OR', 'Pennsylvania'= 'PA', 'Rhode Island'= 'RI', 'South Carolina'= 'SC', 'South Dakota'= 'SD', 'Tennessee'= 'TN', 'Texas'= 'TX', 'Utah'= 'UT', 'Vermont'= 'VT', 'Virginia'= 'VA', 'Washington'= 'WA', 'West Virginia'= 'WV', 'Wisconsin'= 'WI', 'Wyoming'= 'WY')),
                  selectInput("Fitmet", "Fitting method for the correlation analysis", selected = "loess", choices = list("lm"="lm", "glm"="glm", "gam"="gam", "loess"="loess")),
                  hr(),
                  selectInput('Corrdep', 'Dependent Variable for the correlation on GDP and Spy Datasets', choices = list('Spy Data Close Value'= "Close", 'Spy Data Volume'= 'Volume', "US GDP Dataset GDP" = "GDP")),
                  selectInput("CorrInd", "Independent variable for a correlation analysis on GDP and Spy Datasets", selected = "GDP", choices = list('Spy Data Close Value'= "Close", 'Spy Data Volume'= 'Volume', "US GDP Dataset GDP" = "GDP")),
                  selectInput("Fittmet", "Fitting method for the correlation analysis on GDP and Spy Datasets", selected = "loess", choices = list("lm"="lm", "glm"="glm", "gam"="gam", "loess"="loess"))
                  
              ),
              
              mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Map Plot", plotOutput("maptable", height = "100%")),
                              tabPanel("Regression Summary", verbatimTextOutput("summary")),
                              tabPanel("Regression Plot", plotOutput("regresult", height="100%")),
                              tabPanel("Correlation Plot", plotOutput("corresult", height="100%")),
                              tabPanel("Correlation Plot for GDP/SPY Dataset", plotOutput("corrresult", height="100%"))
                              
                  )
              )
    )
)
# Define server logic required for the app
server <- function(input, output, session) {
    
    #Print the first n rows of the dataset.
    output$maptable <- renderPlot({
        GDP_data$Date <- as.Date(GDP_data$Date)
        thedate <- as.Date(input$Gvndate)
        GDP <- subset(GDP_data , Date == thedate, select = AL:US)
        GDP
        
        New <- matrix(nrow = 51, ncol = 2)
        counter = 1
        for (cols in colnames(GDP)){
            New[counter,1] <- as.character(cols) 
            New[counter, 2] <- GDP[[cols]][1]
            counter <- counter + 1
        }
        New <- data.frame(New)
        names(New)[1] <- "state"
        names(New)[2] <- "GDP"
        New$state <- as.character(New$state)
        New$GDP <- as.numeric(as.character(New$GDP))
        New <- New[1:50,]
        New <- cbind(state.name, New)
        New
        plot_usmap(data = New[1:50,], values = "GDP", labels = TRUE) + 
            scale_fill_continuous(
                low = "white", high = "blue", name = "GDP", label = scales::comma
            ) + theme(legend.position = "right")
        
    } , height = 800)  
    
    
    #Regression analysis to predict the dependent variable using all the other variables as independent variables, and print out the summary of the regression  
    output$summary <- renderPrint({
        b <- paste(input$IndVar, collapse = "+")
        formula = as.formula(paste(input$DepVar, '~', b, sep = "" ))
        model <- lm(formula, data = GDP_data)
        summary(model)
    })
    
    output$regresult <- renderPlot({
        b <- paste(input$IndVar, collapse = "+")
        formula = as.formula(paste(input$DepVar, '~', b, sep = "" ))
        model <- lm(formula, data = GDP_data)
        plot_model(model, show.loess.ci = T, show.values = T, show.p = T, se = T, title = "Plot showing coefficients of regression")
    } , height = 800)
    
    output$corresult <- renderPlot({
        formula = as.formula(paste(input$Cordep, '~', input$CorInd, sep = "" ))
        model <- lm(formula, data = GDP_data)
        selectedData <- data.frame(GDP_data[, c(input$CorInd, input$Cordep)])
        
        ggplot(data = selectedData, aes(x = selectedData[, 1], y = selectedData[,2])) + xlab("Correlation independent variable") + ylab("Correlation dependent variable")+
            geom_point(color = 'red') +
            stat_smooth(method = input$Fitmet) + title("Correlation between independent and dependent variable from State GDP Index Data")
    } , height = 800)
    
    output$corrresult <- renderPlot({
        GDP_data$Date <- as.Date(GDP_data$Date)
        usgdp <- usgdp[15:290,]
        spy$Date <- as.Date(spy$Date) + 1
        names(usgdp)[1] <- "Date"
        usgdp$Date <- as.Date(usgdp$Date)
        Newer <- merge(usgdp, spy,  by="Date")
        
        formula = as.formula(paste(input$Corrdep, '~', input$CorrInd, sep = "" ))
        model <- lm(formula, data = Newer)
        selectedData <- data.frame(Newer[, c(input$CorrInd, input$Corrdep)])
        
        ggplot(data = selectedData, aes(x = selectedData[, 1], y = selectedData[,2])) + xlab("Correlation independent variable") + ylab("Correlation dependent variable")+
            geom_point(color = 'brown') +
            stat_smooth(method = input$Fittmet) + title("Correlation between independent and dependent variable from the SPY/GDP data")
    } , height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
