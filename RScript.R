setwd("~/Fall2020/DATA440/R/Data")
getwd()


x <- 1:10
y <- 10:1

plot(x,y)

plot(x, y, type = "l")

plot(x, y, type = "o")

plot(x, y, type = "o", 
     main = "The Path of a Running Boy",
     sub = "units of distance = meters",
     xlab = "longitude", 
     ylab = "latitude")

plot(x, y, type = "b", main = "The Path of a Running Boy", 
     sub = "units of distance = meters", 
     xlab = "longitude", 
     ylab = "latitude",
     lty = 2,
     lwd = .75,
     col = "blue",
     pch = 0,
     cex = 1.5)
#####
x <- 1:100
y <- 1:100

east <- sample(x, size = 10, replace = TRUE)
north <- sample(y, size = 10, replace = TRUE)

symbols(east, north, squares = rep(.75,10), inches = FALSE)

symbols(sample(x, 10, replace = TRUE), 
        sample(y, 10, replace = TRUE), 
        circles = rep(.75,10), 
        inches = FALSE,
        fg = "green",
        add = TRUE)
####
symbols(east, north, squares = rep(.75,10), inches = FALSE)

symbols(sample(x, 10, replace = TRUE), 
        sample(y, 10, replace = TRUE), 
        circles = rep(.75,10), 
        inches = FALSE,
        fg = "green1",
        bg = "beige",
        add = TRUE)

symbols(sample(x, 10, replace = TRUE), 
        sample(y, 10, replace = TRUE), 
        circles = rep(1.5,10), 
        inches = FALSE,
        fg = "green4",
        bg = "beige",
        add = TRUE)

dwellings <- cbind.data.frame(id = 1:10, east, north)

lines(x = dwellings$east,
      y = dwellings$north,
      lty = 2,
      lwd = .75,
      col = "blue")

text(x = dwellings$east,
     y = dwellings$north,
     labels = dwellings$id)

locs <- sample(1:10, 3, replace = FALSE)

lines(x = dwellings[locs, 2],
      y = dwellings[locs, 3],
      lty = 2,
      lwd = .75,
      col = "blue")

# text(x = dwellings$east,
#      y = dwellings$north + 3,
#      labels = dwellings$id)

text(x = dwellings[locs, ]$east, 
     y = dwellings[locs, ]$north + 3,
     labels = dwellings[locs, ]$id)

xspline(x = dwellings[locs, 2],
        y = dwellings[locs, 3],
        shape = -1,
        lty = 2)
title(main="A Person's path between Homes")
######CHALLENGE######

x <- 1:1000
y <- 1:1000

east <- sample(x, size = 50, replace = TRUE)
north <- sample(y, size = 50, replace = TRUE)

symbols(east, north, squares = rep(3,50), inches = FALSE)

symbols(sample(x, 40, replace = TRUE), 
        sample(y, 40, replace = TRUE), 
        circles = rep(3,40), 
        inches = FALSE,
        fg = "green",
        add = TRUE)

symbols(sample(x, 12, replace = TRUE), 
        sample(y, 12, replace = TRUE), 
        circles = rep(6,12), 
        inches = FALSE,
        fg = "green4",
        bg = "beige",
        add = TRUE)


dwellings <- cbind.data.frame(id = 1:50, east, north)

locs <- sample(1:50, 7, replace = FALSE)


text(x = dwellings[locs, ]$east, 
     y = dwellings[locs, ]$north + 3,
     labels = dwellings[locs, ]$id)

xspline(x = dwellings[locs, 2],
        y = dwellings[locs, 3],
        shape = -1,
        lty = 2)
title(main="Challenge: Getting Started")
