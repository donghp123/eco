install.packages("devtools")
install.packages("rdacca.hp")
install.packages("hier.part")
library(vegan)
library(permute)   
library(lattice)
library(ggrepel)  #作图使用此包，和ggplot2一起用，使文字尽可能不重叠
library(ggplot2)
library(devtools)
library(hier.part)
library(scales)
library(rdacca.hp)
library(grid)

sp <- read.csv("data.csv",row.names=1)
#sp <- read.csv("fun.csv",row.names=1)(读取不同数据)
#sp <- read.csv("bac.csv",row.names=1)（读取不同数据）
sp
sp.hel<- decostand(sp, method = 'hellinger')
#环境数据，有时环境数据需要数据转换以满足数据分析要求，但很多文章里土壤pH是不做转换的
fc <- read.csv("environment.csv",row.names=1)
env=log10(fc)


##
spe.rda<-rda(sp.hel~.,env,scale=FALSE)
ii=summary(spe.rda)
ii###查看分析结果
sp=as.data.frame(ii$species[,1:2])*2.5###提取相应变量坐标，乘以5是使图美观，不影响分析
st=as.data.frame(ii$sites[,1:2])*1.8###提取样方坐标，有两种模式，可根据自己数据探索：二选一即可
yz=as.data.frame(ii$biplot[,1:2])*1###提取解释变量坐标

ggplot() +
  geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#（样地号的数据点大小）
  geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                   arrow = arrow(angle=22.5,length = unit(0.3,"cm"),#（数据点箭头大小）
                   type = "closed"),linetype=1, size=1,colour = "red")+
  geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)))+
  geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                   arrow = arrow(angle=22.5,length = unit(0.3,"cm"),
                   type = "closed"),linetype=1, size=1,colour = "blue")+
  geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)))+
  labs(x="RDA1 58.13%",y="RDA2 16.50%")+#（此数据是用ii分析结果中的第一个RDA1和RDA2 数据自己填写）
  geom_hline(yintercept=0,linetype=3,size=1)+ #（横坐标轴）
  geom_vline(xintercept=0,linetype=3,size=1)+#（纵坐标轴）
  theme_bw()+
  theme(panel.grid=element_blank())

 #可以看出，校正R2总是小于R2。校正R2作为被解释方差比例的无偏估计，后
#面的变差分解部分所用的也是校正R2。
r2<-RsquareAdj(spe.rda)
rda_noadj<-r2$r.squared #原始R2，不能直接使用
rda_adj <- r2$adj.r.squared  #校正后的R2乘以RDA1和2的解释度，才是作图用的实际解释度
rda_adj
# RDA所有轴置换检验
anova(spe.rda, permutations = how(nperm = 999))
# 每个典范轴逐一检验
anova(spe.rda, by = "axis", permutations = how(nperm = 999))
# 每个环境因子进行检验,不靠谱
anova(spe.rda, by = "term", permutations=999)
#F value for importance of env variables
anova(spe.rda, by = "margin", permutations=999)
#envfit(spe.rda,env,choices=1,permutations =999) 


o<-rdacca.hp (sp.hel,env,method="RDA",type="adjR2")   # 每个解释变量占总变化量的比例
o



#Mantel test 测试
spe.dist<-vegdist(sp.hel,method = 'bray') 

env.dist=vegdist(env,method = "euclidean") # 整体环境因子数据一起分析，环境因子矩阵一般用欧式距离

# 2.4.2 所有环境因子mantel分析，vegan::mantel()表示使用vegan包中的mantel()函数
env.spe.mantel = vegan::mantel(spe.dist,env.dist,permutations=999,method="pearson")

env.spe.mantel # 查看整体分析结果

single.mantel <-function(spe,env, sim.method, correlation, p.adjust.m){
  library(vegan)
  co = colnames(env)
  factor = c()
  r = c()
  p.value=c()
  spe.dist<-vegdist(spe,method = sim.method,na.rm = TRUE)
  for(elem in 1:length(co)){
    env.dist = vegdist(env[elem], method = "euclidean")
    ad = vegan::mantel(spe.dist, env.dist, permutations = 999,method=correlation);    
    factor = c(factor,co[elem]);    
    r = c(r,ad$statistic);   
    p.value=c(p.value, ad$signif)
  }
  p.adjusted =p.adjust(p.value,method=p.adjust.m) 
  pairw.res = data.frame(factor, r, p.value, p.adjusted) 
  return(pairw.res)
} 
spe.sin.mantel = single.mantel(sp.hel, env, sim.method="bray", correlation = "pearson", p.adjust.m= "fdr") # 多重比较
spe.sin.mantel 

