library('shiny')
library('ggplot2')
library('dplyr')
library('caret')
library('CatEncoders')

data(iris) #importing data

ui <- fluidPage(
  titlePanel("Iris"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("x", 
                  label = "X-Axis",
                  choices = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                  selected = "Sepal.Length"),
      #setting selectable values for x axis
      
      selectInput("y", 
                  label = "Y-Axis",
                  choices = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                  selected = "Sepal.Width"),
      #setting selectable values for y axis
      
      selectInput('z',
                  label = 'Color by',
                  choices = 'Species',
                  selected = 'Species')
      #coloring the dots via species
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot"), #displaying plot
      tableOutput(outputId = 'summary_table')
  )
  )
)

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = iris, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
  })
  
  output$summary_table <- renderTable(
    {
      iris %>%
        group_by(Species) %>%
        summarise(AvgPetalLength = mean(Petal.Length), SD = sd(Petal.Length), n = n())
    })
}

shinyApp(ui = ui, server = server)