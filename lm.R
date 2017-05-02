## Algoritma LM untuk IRIS Dataset
## Ariq Cahya Wardhana / G651160411
##========================================
plot(iris$Petal.Width, iris$Petal.Length, pch=21, bg=c("red","red","blue")[unclass(iris$Species)], xlab="Petal Width", ylab="Petal Length")
abline(lm(Petal.Length ~ Petal.Width, data=iris)$coefficients, col="black")
summary(lm(Petal.Length ~ Petal.Width, data=iris))
