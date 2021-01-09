

ui <- fluidPage(
    
    theme = shinytheme('united'),
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            
            
            
            
        ),
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            plotOutput('projectplot'),
            plotOutput('projectplot2')
            
        )
        
    )
    
)




# Create Shiny app ----
# shinyApp(ui, server)