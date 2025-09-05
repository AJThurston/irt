library(gapminder)
library(ggplot2)
library(magick)
img <- image_graph(res = 96)
datalist <- split(gapminder, gapminder$year)
out <- lapply(datalist, function(data){
  p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
    scale_size("population", limits = range(gapminder$pop)) +
    scale_x_log10(limits = range(gapminder$gdpPercap)) +
    geom_point() + ylim(20, 90) +  ggtitle(data$year) + theme_classic()
  print(p)
})
dev.off()
animation <- image_animate(img, fps = 50)
image_write(animation, "animation.gif")