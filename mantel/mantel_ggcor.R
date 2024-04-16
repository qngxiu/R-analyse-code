##mantel分析
#ggcor包进行mantel分析
library(ggcor)    #ggcor帮助文档 https://cloud.tencent.com/developer/article/1534323
?ggcor
library(dplyr)
library(vegan)
library(ggplot2)
#install.packages("ade4")
library(ade4)
# 载入示例数据
data("varechem", package = "vegan")      #环境因子数据
data("varespec", package = "vegan")      #加载物种数据

# Mantel.test 检验计算矩阵相关性
mantel <- mantel_test(varespec, varechem, mantel.fun = 'mantel.randtest',spec.dist.method = 'bray', env.dist.method = 'euclidean', 
                      spec.select = list(Spec01 = 1:7,
                                         Spec02 = 8:18,
                                         Spec03 = 19:37
                      )) %>% 
  mutate(r_value = cut(r, breaks = c(-Inf, 0.25, 0.5, Inf), 
                       labels = c('<0.25', '0.25-0.5', '>=0.5'), right = FALSE),
         p_value = cut(p.value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
                       labels = c('<0.001', '0.001-0.01', '0.01-0.05', '>=0.05'), right = FALSE))

quickcor(varechem, type = "upper") +
  geom_square() +
  anno_link(aes(colour = p_value, size = r_value), data = mantel) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = c("#D95F02", "#1B9E77", "#A2A2A288")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))

ggsave(filename = "mantel.tiff", device="pdf", width=10, height=6,dpi = 300,path = "地址")


