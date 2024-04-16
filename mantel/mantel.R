##LinkET包进行mantel分析
rm(list = ls())
# install.packages("devtools") 
#devtools::install_github("Hy4m/linkET", force = TRUE)           ##这两步是github中安装包，因为包还没正式上线
#packageVersion("linkET")
library(linkET)
library(ggplot2)
library(dplyr)
#data("varechem", package = "vegan")
#data("varespec", package = "vegan")


varechem <- read.csv("理化性质.csv",header = T,row.names = 1)
varespec <- read.csv("otutab.csv",header = T,row.names = 1)
varespec <- as.data.frame(t(varespec))


mantel <- mantel_test(varespec, varechem,                              #这里注意，物种一定要在第一个，环境因子在第二个
                      spec_select = list(
                        Bacteria = 1:948
                      ))             #这里根据自己需求不同修改

mantel <- mutate(mantel, rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
                 pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                          labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))                #根据自己划分的需求不同进行修改


c <- qcorrplot(correlate(varechem), type = "upper", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd),                #画连接线，颜色由p值决定，粗细由r值决定
              data = mantel,                              #数据为matel，即上文中的模型
              curvature = nice_curvature(0.2)) +             #决定连接线的曲率，也就是线的弯曲程度
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"), 
                       values = scales::rescale(c(-1, -0.5, 0, 0.5, 1)))+
  
  #scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +       #用RColorBrewer包中的brewer.pal()函数进行一个渐变色填充，其中有11个色，颜色方案 名字叫RdBu，相关颜色可以在https://zhuanlan.zhihu.com/p/395727226中查看
  scale_size_manual(values = c(0.5, 1, 2)) +                                   #应该是控制线的粗细
  scale_colour_manual(values = color_pal(3)) +                                #元素的颜色由 color_pal 函数生成的调色板控制，调色板中包含 3 种颜色。
  guides(size = guide_legend(title = "Mantel's r",                            #guide函数用于添加图例
                             override.aes = list(colour = "grey35"),           
                             order = 2),                                      #表示添加一个大小图例，图例的标题为 “Mantel’s r”，覆盖样式为灰色，顺序为 2
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),                                    #添加一个颜色图例，图例的标题为 “Mantel’s p”，覆盖样式为大小为 3，顺序为 1
         fill = guide_colorbar(title = "Pearson's r", order = 3))              #添加一个填充颜色图例，图例的标题为 “Pearson’s r”，顺序为 3。

#  theme(legend.spacing.y = grid::unit(0.1, "lines"),                          #设置图例行间距
#      legend.key.size = grid::unit(0.5, "lines"))                            #设置图例大小 
c
ggsave(filename = "matel_Ba.pdf", device="pdf", width=10, height=6,dpi = 600)
