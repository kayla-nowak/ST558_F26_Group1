library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  "Add a title panel here!",
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      "Put your selectize inputs here!",
      "Give them internal IDs of corr_x and corr_y.",
      "Note the vector with these names comes from the helpers.R files. The object is called `numeric_vars`. Make sure you don't assign the same initial value to both inputs!",
      br(),
      "Place your radio buttons here! One radio button for each variable we may subset on. Set the internal IDs for these to be hhl_corr, fs_corr, and schl_corr.",
      "Notice that you can use choiceNames and choiceValues to have different values show vs the values you use internally for comparisons. There are 'internal' values already used in the server file - the first large commented section - so you should set the internal values to match those!",
      h2("Select a Sample Size"),
      "Put your slider for sample size here. Give this an ID of corr_n.",
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      "Add a plotOutput here for the scatter plot!",
      conditionalPanel("input.corr_sample", #only show if a sample has been taken
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    #################################################
    ##Correlation tab
    #This code makes sure the select boxes update so they can't select the same variable in both!
    #first, update the 'y' selections available
    observeEvent(input$corr_x, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_x)]
        updateSelectizeInput(session,
                             "corr_y",
                             choices = choices,
                             selected = corr_y)
      }
    })
    #now, update the 'x' selections available
    observeEvent(input$corr_y, {
      corr_x <- input$corr_x
      corr_y <- input$corr_y
      choices <- numeric_vars
      if (corr_x != corr_y){
        choices <- choices[-which(choices == corr_y)]
        updateSelectizeInput(session,
                             "corr_x",
                             choices = choices,
                             selected = corr_x)
      }
    })
    



    #Create a reactiveValues() object called sample_corr
    #this object should have two elements, corr_data and corr_truth
    #both should be set to null to start with!



    # ##############################################################
    # #Uncomment the next large block of code to go in an
    # #observeEvent() to look for the action button (corr_sample)
    # #Note you can highlight and bulk comment/uncomment (ctrl+shift+c or similar on mac)
    # 
    #   if(input$hhl_corr == "all"){
    #     hhl_sub <- HHLvals
    #   } else if(input$hhl_corr == "english"){
    #     hhl_sub <- HHLvals["1"]
    #   } else if(input$hhl_corr == "spanish"){
    #     hhl_sub <- HHLvals["2"]
    #   } else {
    #     hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    #   }
    # 
    #   if(input$fs_corr == "all"){
    #     fs_sub <- FSvals
    #   } else if(input$fs_corr == "yes"){
    #     fs_sub <- FSvals["1"]
    #   } else {
    #     fs_sub <- FSvals["2"]
    #   }
    # 
    #   if(input$schl_corr == "all"){
    #     schl_sub <- SCHLvals
    #   } else if(input$schl_corr == "no_hs"){
    #     schl_sub <- SCHLvals[c("0", "01", "02", "03", "04",
    #                            "05", "06", "07", "08", "09",
    #                            "10", "11", "12", "13", "14", "15")]
    #   } else if(input$schl_corr == "hs"){
    #     schl_sub <- SCHLvals[as.character(16:19)]
    #   } else {
    #     schl_sub <- SCHLvals[as.character(20:24)]
    #   }
    # 
    #   corr_vars <- c(input$corr_x, input$corr_y)
    # 
    #   subsetted_data <- my_sample |>
    #     filter(#cat vars first
    #       HHLfac %in% hhl_sub,
    #       FSfac %in% fs_sub,
    #       SCHLfac %in% schl_sub
    #     ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
    #     {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
    #     {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
    #     {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
    #     {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
    #     {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
    #     {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
    #     {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
    #     {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
    #     {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    # 
    #   index <- sample(1:nrow(subsetted_data),
    #                   size = input$corr_n,
    #                   replace = TRUE,
    #                   prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    #   #***You now need to update the sample_corr reactive value object***
    #   #the corr_data argument should be updated to be the subsetted_data[index,]
    #   #the corr_truth argument should be updated to be the correlation between
    #   #the two variables selected. This can be found with this code:
    #   #cor(sample_corr$corr_data |> select(corr_vars))[1,2]
    ####################################################################



    # #Create a renderPlot() object to output a scatter plot
    # #Use the code below to validate that data exists, (this goes in the renderPlot and you'll need 
    # #to install the shinyalert package if you don't have it) and then create the appropriate
    # #scatter plot
    #   validate(
    #     need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    #   )
    #   ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
    #     geom_point()


    #This code does the correlation guessing game! Nothign to change here
    observeEvent(input$corr_submit, {
      close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
      if(close){
        shinyalert(title = "Nicely done!",
                   paste0("The sample correlation is ", 
                          round(sample_corr$corr_truth, 4), 
                          "."),
                   type = "success"
        )
      } else {
        if(input$corr_guess > sample_corr$corr_truth){
          shinyalert(title = "Try again!",
                     "Try guessing a lower value.")
        } else {
          shinyalert(title = "Try again!",
                     "Try guessing a higher value.")
        }
      }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
