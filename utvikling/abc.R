### Komparativ statikk:
library(ggplot2)


qplot(data=iris, 100S*epal.Length, Sepal.Width)




y <- eval(parse(text=expression('(k/n)^(alpha)')),list(alpha=0.5,k=1,n=1:10))   

plottable <- data.frame(y, x=1:10)

abc <- ggplot(data=plottable) + geom_line(aes(x=x, y=y), arrow=arrow()) + theme_classic()
abc



