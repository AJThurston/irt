library(ggplot2)
library(formattable)
library(gridExtra)
setwd("C:\\Users\\AJ Thurston\\Desktop\\combplots")
library(readxl)
gifpars <- read_excel("C:/Users/AJ Thurston/Desktop/gifpars.xlsx")


for (i in 1:nrow(gifpars)){ 

a = as.numeric(gifpars[i,1])/10
b = as.numeric(gifpars[i,2])
c = 0  


a = formattable(a, digits = 2, format = "f")
b = formattable(b, digits = 2, format = "f")
c = formattable(c, digits = 2, format = "f")
t = seq(-3, 3, by=.01)
p = c+((1-c)/(1+exp(-1.7*a*(t-b))))
data = data.frame(t,p)

plot = qplot(t, p, data=data) + 
      annotate("text", x = -2.5, y = 1, label = "@AJThurston", fontface = "bold", size = 4) +
      annotate("text", x = -2.5, y = .9, label = paste0("a = ",a), fontface = "bold", size = 8) +
      annotate("text", x = -2.5, y = .8, label = paste0("b = ",b), fontface = "bold", size = 8) +
      annotate("text", x = 1.7, y = .1, label = paste0(gifpars[i,3]), fontface = "bold", size = 8)+
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
plot

    plotname = paste0("plot",i, ".png")
    ggsave(plotname, dpi = 150, width = 7, height = 5, units = "in")
}