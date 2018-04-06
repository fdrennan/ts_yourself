
library(shiny)
library(imager)
library(dplyr)
library(ggplot2)
library(scales)
library(TSP)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Traveling Salesman Yourself"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("bins",
                    "Put in a link:",
                   "https://i.pinimg.com/736x/ea/8f/f2/ea8ff210d741d8edd3375ef65285c8e5.jpg"),
         numericInput("range", "Enter Threshold", 50, min = 0, max = 100)
      ), 
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     file=input$bins
     if (!file.exists(file)) download.file(urlfile, destfile = file, mode = 'wb')
     
     # Load, convert to grayscale, filter image (to convert it to bw) and sample
     load.image(file) %>% 
       grayscale() %>%
       threshold(paste0(input$range, "%")) %>%
       as.cimg() %>% 
       as.data.frame()  %>% 
       sample_n(8000, weight=(1-value)) %>% 
       select(x,y) -> data
     
     # Compute distances and solve TSP (it may take a minute)
     as.TSP(dist(data)) %>% 
       solve_TSP(method = "arbitrary_insertion") %>% 
       as.integer() -> solution
     
     # Create a dataframe with the output of TSP
     data.frame(id=solution) %>% 
       mutate(order=row_number()) -> order
     
     # Rearrange the original points according the TSP output
     data %>% 
       mutate(id=row_number()) %>% 
       inner_join(order, by="id") %>% arrange(order) %>% 
       select(x,y) -> data_to_plot
     
     # A little bit of ggplot to plot results
     ggplot(data_to_plot, aes(x,y)) +
       geom_path() +
       scale_y_continuous(trans=reverse_trans())+
       coord_fixed()+
       theme_void()
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

