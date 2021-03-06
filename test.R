install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

head(mpg)

dim(mpg)

str(mpg)
summary(mpg)
View(mpg)

mpg %>%
  group_by(manufacturer) %>%
  summarise(mean.hwy=mean(hwy)) %>%
  arrange(desc(mean.hwy))
  
lm.mpg <- lm(data=mpg, hwy ~ displ)
summary(lm.mpg)

qplot(data = mpg, x = displ, y = hwy)


head(mpg)
mean(mpg$hwy)
max(mpg$hwy)
min(mpg$hwy)

hist(mpg$hwy)

a <- 1
b <- 2
c <- 3
ab <- 3.5

a+b
a/b

d <- c(1,2,3,4,5)
e <- c(1:5)
f <- seq(1,5)
g <- seq(1,10,by=2)
d+2
d
d+e
a2 <- "a"
b2 <- "text"
c2 <- "Hello world!"
c2
d2 <- c("a", "b", "c")
d2
e2 <- c("Hello!", "World", "is", "good!")
e2
b2+2
a2+b2


a <- c(1,2,3)
mean(a)
max(a)
min(a)
summary(a)
b <- c("a", "a", "b", "c")
library(ggplot2)
qplot(b)
e2
paste(e2,collapse = " ")
e3_paste <- paste(e2,collapse = ',')
e2_paste

