install.packages("corrplot")
install.packages('PerformanceAnalytics')
library('corrplot')

dat <- read.csv('test1.csv',header = T)
dat <- as.matrix(dat)

corr <- cor(dat)

corrplot(corr)
corrplot(corr, tl.col = 'black')
corrplot(corr, tl.col = 'black', order = 'hclust')

library( PerformanceAnalytics)
chart.Correlation(corr,histogram=T,pch=19,panel=panel.smooth)

corrplot(corr,method = "color",addCoef.col="grey",col.axis="black",col.lab="black")
corr1 <- cor.mtest(dat)
corr1$p

corrplot(corr, tl.col = 'black', order = 'hclust', p.mat = corr1$p, insig = 'blank')

corrplot(corr, tl.col = 'black', order = 'hclust', p.mat = corr1$p, 
         insig = 'label_sig', sig.level = c(0.001,0.01,0.05), pch.cex = 0.9,
         pch.col = 'green')

corrplot(corr, tl.col = 'black', order = 'hclust', p.mat = corr1$p, 
         insig = 'label_sig', sig.level = c(0.001,0.01,0.05), pch.cex = 0.9,
         pch.col = 'green', type = 'lower')

chart.Correlation(df,histogram = T,pch=19)


corrplot(corr, tl.col = 'black', order = 'hclust', p.mat = corr1$p, 
         insig = 'label_sig', sig.level = c(0.001,0.01,0.05), pch.cex = 0.9,
         pch.col = 'green', type = 'lower', method = 'color')
corrplot(corr)

#以下是各种热图类型

M <- cor(dat)
corrplot(M, method = "circle")

corrplot(M, method = "square")

corrplot(M, method = "ellipse")

corrplot(M, method = "number") # 显示相关性系数

corrplot(M, method = "shade")

corrplot(M, method = "color")

corrplot(M, method = "pie")

corrplot(M, type = "upper")

corrplot.mixed(M)

corrplot.mixed(M, lower.col = "black", number.cex = .7)

corrplot(M, order = "alphabet")

corrplot(M, order = "hclust", addrect = 2)

# Change background color to lightblue
corrplot(M, type = "upper", order = "hclust",
         col = c("black", "white"), bg = "lightblue")

res1 <- cor.mtest(mtcars, conf.level = .95)
res2 <- cor.mtest(mtcars, conf.level = .99)

## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1$p, sig.level = .2)

corrplot(M, low = res1$lowCI, upp = res1$uppCI, order = "hclust",
         rect.col = "navy", plotC = "rect", cl.pos = "n")

corrplot(M, p.mat = res1$p, low = res1$lowCI, upp = res1$uppCI,
         order = "hclust", pch.col = "red", sig.level = 0.01,
         addrect = 3, rect.col = "navy", plotC = "rect", cl.pos = "n")

