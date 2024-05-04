library(shiny)
library(ggplot2)

server <- function(input, output) {
  output$distPlot = renderPlot({
    t = seq(-3, 3, by=.01)
    p = input$c+((1-input$c)/(1+exp(-1.7*input$a*(t-input$b))))
    data = data.frame(t,p)
    plot = qplot(t, p, data=data)
    plot + 
      annotate("text", x = 2, y = .5, label = "@AJThurston", size = 5)+
      scale_y_continuous(
        name="Prob. Pos. Response Given Theta p(ui=1|T)", 
        limits=c(0,1), 
        breaks=seq(0,1,.5)
        ) +
      scale_x_continuous(
        name="Trait Level (T)", 
        limits=c(-3,3), 
        breaks=seq(-3,3,1)
        )+
            theme(
        axis.title.y = element_text(face = "bold", size = 14, angle = 90),
        axis.title.x = element_text(face = "bold", size = 14, angle = 0),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
      )
  }, height = 400, width = 600)
}
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("3PL Parameters"),
      sliderInput("a", label = h5("a"), value = 1, step = .1, min = 0, max = 10),
      sliderInput("b", label = h5("b"), value = 0, step = .01, min = -3, max = 3),
      sliderInput("c", label = h5("c"), value = 0, step = .01, min = 0, max = 1)
    ),
      mainPanel(plotOutput("distPlot"))
  )
)
shinyApp(ui = ui, server = server)
