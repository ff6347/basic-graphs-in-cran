# install.packages("RColorBrewer")
# install.packages("gcookbook")
# install.packages("gridExtra")
# install.packages("reshape2")
#
# setwd("/Users/fmz/Desktop/basic-graphs-in-cran/")

 getwd()
library(gcookbook)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(reshape2) # for melt

# display.brewer.all()
summary(diamonds)

#bar graph
# #png(filename="images/barplot.png",width=2000,height=1000)
theme_set(theme_grey(base_family="Vollkorn"))

bg <- ggplot(diamonds, aes(clarity)) + geom_bar(fill="blue", alpha=0.2,color="black")

bg <- bg + scale_fill_brewer(palette="Spectral") + labs(title="Bar Chart")

# bg
# dev.off()

#bar graph grouped
# #png(filename="images/grouped-barplot.png",width=2000,height=1000)

cabbage_exp

gbg <- ggplot(cabbage_exp, aes(x=Date,y=Weight, fill=Cultivar )) + geom_bar(position="dodge",stat="identity",alpha=0.5,color="black")
gbg <- gbg + scale_fill_brewer(palette="Spectral")  + labs(title="Grouped Bar Chart")
# gbg
# dev.off()


# stacked barplot
# #png(filename="images/stacked-barplot.png",width=2000,height=1000)

bp <- ggplot(diamonds, aes(clarity, fill = cut, order = -as.numeric(cut))) + geom_bar()
bp <- bp + scale_fill_brewer(palette="Spectral")  + labs(title="Stacked Bar Chart")
# bp
# dev.off()

# area
#
# #png(filename="images/area-chart.png",width=2000,height=1000)

sunspotyear <- data.frame(Year=as.numeric(time(sunspot.year)),Sunspots= as.numeric(sunspot.year))
sunspotyear

ac <- ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(fill="yellow",alpha=0.2) + geom_line()
ac <- ac + scale_fill_brewer(palette="Spectral")  + labs(title="Area Chart")
# ac

# dev.off()
# stacked area
# #png(filename="images/stacked-area-chart.png",width=2000,height=1000)
ap <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
ap <- ap + scale_fill_brewer(palette="Spectral")  + labs(title="Stacked Area Chart")
# ap <- ap +  coord_polar(theta="y")
# ap
# dev.off()

# line
# #png(filename="images/line-chart.png",width=2000,height=1000)

lp <- ggplot(diamonds, aes(x=carat,y=depth)) +geom_line() +  scale_fill_brewer(palette="Spectral")+ labs(title="Line Chart")
# lp
# dev.off()

# ribbon aka Bivariate Area Chart

clim <- subset(climate, Source == "Berkeley", select=c("Year","Anomaly10y","Unc10y"))
summary(clim)
clim

rp <- ggplot(clim, aes(x=Year,y=Anomaly10y)) + geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y,alpha=0.2 )) + geom_line() + labs(title="Bivariate Area Chart")
# rp
# dev.off()

#scatterplot
#
#
# #png(filename="images/scatterplot.png",width=2000,height=1000)

heightweight
summary(heightweight)

sp <- ggplot (heightweight, aes(x=ageYear,y=heightIn,colour=sex,size=weightLb)) + geom_point() + scale_fill_brewer(palette="Spectral") + labs(title="Scatter Plot")
# sp

# dev.off()

# donut

# boxplot
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p <- p + geom_boxplot() + labs(title="Box Plot")


# contour
#
# Generate data
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")

# Basic plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v <- v+ stat_contour(binwidth = 2) +  labs(title="Contour Chart")

#polygon

# When using geom_polygon, you will typically need two data frames:
# one contains the coordinates of each polygon (positions),  and the
# other the values associated with each polygon (values).  An id
# variable links the two together

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
  0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
  2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

# piechart
# http://uweziegenhagen.de/?p=2811
df <- data.frame(
  Legende = c("male", "female", "child"),
  werte = c(20, 20,60)
)

blue <- rgb(26/255, 115/255, 186/255, 1)
yellow <- rgb(255/255, 219/255, 67/255, 1)
orange <- rgb(181/255, 75/255, 5/255, 1)

pc <- ggplot(df, aes(x = "", y = werte, fill = Legende)) +
  geom_bar(width = 1, stat = "identity") +
    scale_fill_manual(values =  c(blue, yellow, orange)) +
    coord_polar("y", start = 0) +
  labs(title = "Pie Chart")  +
  xlab("X-Achse") +
  ylab("Y-Achse")



# bp <- ggplot(diamonds, aes(clarity, fill = cut, order = -as.numeric(cut))) + geom_bar()
# bp <- bp + scale_fill_brewer(palette="Spectral")
# # bp <- bp +  coord_polar(theta="y")
# bp

# Create test data.
# http://stackoverflow.com/a/13636037/1770432
dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))

# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

#png("donut-chart.png", height=1000, width=2000)
p1 <- ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
     geom_rect(color="black",alpha= 0.9) +
     coord_polar(theta="y") + scale_fill_brewer(palette="Spectral") +
     xlim(c(0, 4)) +
     labs(title="Donut/Ring Chart")
# p1

# p2 <- ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
#      geom_rect(colour="grey30") +
#      coord_polar(theta="y") +
#      xlim(c(0, 4)) +
#      theme_bw() +
#      theme(panel.grid=element_blank()) +
#      # theme(axis.text=element_blank()) +
#      # theme(axis.ticks=element_blank()) + scale_fill_brewer(palette="Spectral") +
#      labs(title="Customized ring plot")
# p2

png(file="all-plots.png", width=1920, height=1080, units="px")
grid.arrange(bg,gbg,bp,ac,ap,lp,rp,sp,p, v,p1, pc)

dev.off()