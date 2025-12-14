setwd('E:/06GBD写作/000stroke and GBD2023/202512正式分析结果/results')
library(easyGBDR)
df <- GBDread(folder=T,foldername ='data')
write.csv(df,'2023stroke.csv')

unique(df$measure)
unique(df$location)
unique(df$sex)
unique(df$age)
unique(df$cause)
unique(df$metric)
unique(df$year)

##年龄0-95+
df1 <- df |> filter( age %in% c("0 to 4","5 to 9", "10 to 14",
                                "15 to 19", "20 to 24", "25 to 29", 
                                "30 to 34", "35 to 39", "40 to 44", 
                                "45 to 49", "50 to 54", "55 to 59", 
                                "60 to 64", "65 to 69", "70 to 74", 
                                "75 to 79", "80 to 84", 
                                "85 to 89", "90 to 94", "95 plus",
                                "All ages","Age-standardized")) 

library(GlobalBurdenR)
library_required_packages()


##1.读取官网下载的原始数据：####
# data=unzip_and_merge_csv_files_vroom_progress('./stroke2023data/cause data1990and2023')
# data=gbd_standardize_location_names(df1)#校正名称拼写错误等问题
##标准化Location名字
data=debug_gbd_data_check(df)#或者用gbd_rename_column(data)
unique(data$age)
##
write.csv(data,'2023stroke.csv')

##2.三线表####
location=unique(data$location)
location=c("China","United States of America","Japan","Republic of Korea" )
order=c("China","United States of America","Japan","Republic of Korea" )

t2 <- GBD_table(
  input_path = "2023stroke.csv",
  measure = "Deaths",
  locations = location,
  regions_order = order,
  year1 = 1990,
  year2 = 2023,
  output_path = 'Table1death.docx'
)
t2

unique(df$measure)
GBD_table(
    input_path = "2023stroke.csv",
    measure = "DALYs (Disability-Adjusted Life Years)",
    locations = location,
    regions_order = order,
    year1 = 1990,
    year2 = 2023,
    output_path = 'Table1DALYs.docx'
)
GBD_table(
    input_path = "2023stroke.csv",
    measure = "Prevalence",
    locations = location,
    regions_order = order,
    year1 = 1990,
    year2 = 2023,
    output_path = 'Table1Prevalence.docx'
)
GBD_table(
    input_path = "2023stroke.csv",
    measure = "Incidence",
    locations = location,
    regions_order = order,
    year1 = 1990,
    year2 = 2023,
    output_path = 'Table1Incidence.docx'
)

##EAPC结果可视化####
EAPC <- GBDeapc(data=df1 , #指定用于建模的数据
                      rei = F, #数据是否含有rei变量
                      EAPC_95CI = T, #是否生成EAPC_95CI这一列
                      digits = 2,  #保留的小数点个数
                      sep = " to ")  
write.csv(EAPC,"EAPC.csv")

library(dplyr)
# 定义顺序向量
measure_order <- c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)","Deaths")

EAPCplotdata <- EAPC |> 
    filter(age %in% c("Age-standardized")) |>#filter(metric=="Rate")|>
    # 使用match函数将location匹配到指定顺序，然后排列
    arrange(match(location, measure_order))


EAPCplotdata$measure <- factor(EAPCplotdata$measure, levels = measure_order)

# 假设您的数据中有一个变量用于区分三个子图（比如"Category"）
ggplot(EAPCplotdata, aes(x = location, y = EAPC, fill = sex)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(
        aes(ymin = LCI, ymax = UCI),
        position = position_dodge(0.8),
        width = 0.25
    ) +
    # 添加分面，创建三个子图
    facet_wrap(~ measure, nrow = 2) +  # 假设Category变量有A、B、C三个水平
    scale_fill_jama()+scale_color_jama() +theme_bw()+
    labs(
        #title = "EAPC with Confidence Intervals",
        x = "Location",
        y = "EAPC Value",
        fill = "Sex"
    ) +
    #theme_minimal() +
    theme(
        legend.position = "top",
        axis.text.x = element_text(#angle = 45, 
            hjust = 1))
ggsave("./202512正式分析结果/EAPCplot.pdf",width = 10,height = 8,dpi = 300,units = "in")


##年龄变化趋势--------####
unique(df1$age)
plot1data <- df1 |> 
    filter(year == 2023, metric == "Rate") |> 
    filter(sex %in% c("Female", "Male"))|> filter(!age %in%c("All ages","Age-standardized"))

unique(df1$age)
age_order <- c("95 plus","90 to 94","85 to 89","80 to 84","75 to 79","70 to 74",
               "65 to 69","60 to 64","55 to 59","50 to 54","45 to 49","40 to 44",
               "35 to 39","30 to 34","25 to 29","20 to 24","15 to 19","10 to 14",
               "5 to 9","0 to 4")
  

# 将男性的值转为负数
plot1data <- plot1data %>%
    mutate(val = ifelse(sex == "Female", -val, val))

# 设置measure因子的顺序
plot1data$measure <- factor(plot1data$measure, levels = measure_order)
plot1data$age <-  factor(plot1data$age, levels = rev(age_order))

# 创建图表 - 颠倒x轴和y轴
ggplot(plot1data, aes(x = val, y = age, fill = location)) +  # 交换x和y
    geom_col(position = "stack", orientation = "y") +  # 添加orientation参数
    facet_wrap(~ measure, nrow = 2, scales = "free_x") +  # 改为free_x
    scale_fill_jama() + scale_color_jama() +
    # 添加垂直参考线，标记0值位置
    #geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    # 添加性别标签注释
    annotate("text", x = Inf, y = Inf, label = "Male", 
             hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
    annotate("text", x = -Inf, y = Inf, label = "Female", 
             hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
    labs(x = "Rate per 100,000 population",  # 交换x和y轴标签
         y = "Age",
         fill = "Location") +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 0, hjust = 1))

ggsave("2.1ageTrend.pdf",width = 10,height = 8,dpi = 300,units = "in")

write.csv(plot1data,"agetrenddata.csv")

##年份变化趋势######

# 中国 vs 美国 青年/非青年卒中的GBD负担和危险因素分析比较
library(tidyverse)
library(easyGBDR)
library(dplyr)
library(ggsci)
library(patchwork)
library(ggplot2)
GBD_edition(edition = 2023)


plot3data <- df1 |> filter(age=="Age-standardized")
measure_order <- measure_order

plot3data$measure <- factor(plot3data$measure, levels = measure_order)


ggplot(plot3data, aes(x = year, y = val, colour = location)) +
    geom_line(size = 0.8) +geom_point()+
    scale_color_jama() +
    facet_grid(rows = vars(measure), cols = vars(sex), scales = "free_y") +
    labs(
        x = "Year",
        y = "ASR per 100,000 population",#paste("ASR per ", 10^5, " population")Rate per 100,000 population
        colour = "Country"
    ) +
    # 使用theme_bw()作为基础，它已经包含面板边框
    theme_bw() +
    theme(
        legend.position = "top",
        strip.background = element_rect(fill = "lightgray"),
        # 确保边框是黑色
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        # 调整网格线
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        # 调整面板间距
        panel.spacing = unit(0.8, "lines")
    )


ggsave("3YearTrend.pdf",width = 10,height = 8,dpi = 300,units = "in")


write.csv(plot3data,"yeartrenddata.csv")



##APC分析####



config_stata("D:/Stata17", version = 17, stata_type = c("MP"))

library(tidyverse)
apc1 <- GBDapc_interact(df1,
                        startyear = 1994,#需要30年数据，因此数据选择从1992年开始
                        endyear = 2023)


# 2)结果整理表格
table1 <- apcinteract_table(
    data= apc1,
    type = "age_period",#同样三选一#"age_period", "age_cohort", "period_cohort"
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#可多元素，记得使用c()
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),#可多元素
    cause_name = "Ischemic stroke",#可多元素
    rei_name = NULL,#可多元素
    sex_name = "Both"#可多元素
)

write.csv(table1, file = "APC_age_period.csv")


table2 <- apcinteract_table(
    data= apc1,
    type = "age_cohort",#同样三选一#"age_period", "age_cohort", "period_cohort"
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#可多元素，记得使用c()
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),#可多元素
    cause_name = "Ischemic stroke",#可多元素
    rei_name = NULL,#可多元素
    sex_name = "Both"#可多元素
)

write.csv(table2, file = "APC_age_cohort.csv")

table3 <- apcinteract_table(
    data= apc1,
    type = "period_cohort",#同样三选一#"age_period", "age_cohort", "period_cohort"
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#可多元素，记得使用c()
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),#可多元素
    cause_name = "Ischemic stroke",#可多元素
    rei_name = NULL,#可多元素
    sex_name = "Both"#可多元素
)

write.csv(table3, file = "APC_period_cohort.csv")


#3)建模结果可视化

ggapc_interaction(
    data = apc1,
    type = c("age_period"),#三选一，c("age_period", "age_cohort", "period_cohort")
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#c("China","Japan","Philippines","Viet Nam","China")
    measure_name = "DALYs (Disability-Adjusted Life Years)",#c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",
    rei_name = NULL,#没有rei数据时填NULL，不然报错：错误于data$rei: 类别为'closure'的对象不可以取子集;有则填rei名称
    sex_name = c("Both"),
    line_size = 1 #调整线宽度
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.1DALYS_age_period.pdf", width = 10, height =8, units = "in", dpi = 300)

#####
ggapc_interaction(
    data = apc1,
    type = c("age_cohort"),#三选一，c("age_period", "age_cohort", "period_cohort")
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#c("China","Japan","Philippines","Viet Nam","China")
    measure_name = "DALYs (Disability-Adjusted Life Years)",#c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",
    rei_name = NULL,#没有rei数据时填NULL，不然报错：错误于data$rei: 类别为'closure'的对象不可以取子集;有则填rei名称
    sex_name = c("Both"),
    line_size = 1 #调整线宽度
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.2DALYS_age_cohort.pdf", width = 10, height =8, units = "in", dpi = 300)

######
ggapc_interaction(
    data = apc1,
    type = c("period_cohort"),#三选一，c("age_period", "age_cohort", "period_cohort")
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),#c("China","Japan","Philippines","Viet Nam","China")
    measure_name = "DALYs (Disability-Adjusted Life Years)",#c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",
    rei_name = NULL,#没有rei数据时填NULL，不然报错：错误于data$rei: 类别为'closure'的对象不可以取子集;有则填rei名称
    sex_name = c("Both"),
    line_size = 1 #调整线宽度
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.3DALYS_period_cohort.pdf", width = 10, height =8, units = "in", dpi = 300)


###分解分析####

unique(df1$age)
# 筛选 Global 和 5个SDI quantile进行建模。这里只是为了演示，实际操作过程中你可以全部location都放进去建模的

unique(df1$metric)
names(df1)
measure_order <- measure_order 
temp <- df1|>filter(sex=="Both")#,age=="Age-standardized",metric=="Rate")
temp$measure <- factor(temp$measure, levels = measure_order)


min(temp$val)

# 构建模型
decomposition_result <- GBDdecomposition(
    data=temp, #指定需要建模的数据
    byear = 1990,#基线年份，用于比较的基线
    compareyear = 2023, #终止年份
    startage = 0, #最小年龄段的下限
    endage = 95,
    percent = "overall difference"
)

decomposition_result$measure <- factor(decomposition_result$measure, levels = measure_order)


#导出表格数据
decom_table <- GBDdecomposition_table(
    data=decomposition_result,
    digits = 2,
    measure_name=unique(temp$measure),
    location_name=unique(temp$location),
    sex_name=unique(temp$sex),
    rei_name = NULL,
    cause_name=unique(temp$cause)
)


# install.packages("writexl")
library(writexl)

# 导出为Excel文件
write_xlsx(decom_table, "decomposition_results.xlsx")

#导出图片：
ggdecomposition(
    data=decomposition_result,
    measure_name=c("Incidence","Prevalence","Deaths",
                   "DALYs (Disability-Adjusted Life Years)"),#unique(temp$measure),
    location_name=unique(temp$location),
    sex_name=unique(temp$sex),
    rei_name = NULL,
    cause_name=unique(temp$cause),
    percent = F  #decomposition_result构建的是percent = "overall difference"，因此这里只能填写F
)+ theme_bw()+facet_wrap(~measure,ncol=2,scales = "free_x")+
    scale_fill_jama()+scale_color_jama() 

ggsave("4decomposition.pdf",width = 10,height = 8,dpi = 300,units = "in")





##BAPC########


# remove.packages("fmesher")
# remove.packages(c("INLA", "sf"))
# 重新安装
# install.packages("fmesher",version="0.3.0")
options(timeout = 6000)


#INLA_24.03.29.zip
#fmesher_0.3.0.zip


library(tidyverse)
library(easyGBDR)
library(dplyr)
library(ggsci)
library(patchwork)
library(ggplot2)
# install.packages("fmesher")
library(fmesher)
library(future.apply)
library(pbapply)
library(parallel)
library(doParallel)
library(data.table)

GBD_edition(edition = 2023)



# 构建bapc模型####
plot3data<-  df1
unique(plot3data$location)
measure_order <-measure_order

plot3data$measure <- factor(plot3data$measure, levels = measure_order)

####group==total
bapc_results2 <- GBDbapc_prediction(
    data = plot3data,
    measure_name = measure_order, 
    cause_name = "Ischemic stroke",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    rei_name = NULL,
    By_sex = F,
    predyear = 2050,
    full_age_adjusted = T,
    rate_lessen = NULL,
    pop_predict = "GBD"
)

write.csv(bapc_results2[["all_age_projection"]],"bapc_all_age_projection.csv")
write.csv(bapc_results2[["crude_rate"]],"bapc_crude_rate.csv")
write.csv(bapc_results2[["ASR"]],"bapc_ASR.csv")
write.csv(bapc_results2[["Age_standardized_projection"]],"bapc_Age_standardized_projection.csv")
write.csv(bapc_results2[["age_specific_rate"]],"bapc_age_specific_rate.csv")
write.csv(bapc_results2[["age_specific_projection"]],"bapc_age_specific_projection.csv")







p1 <-  ggprediction_ASR(
    data = bapc_results2,
    CI = T,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Incidence",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

p11 <-  ggprediction_number(
    data = bapc_results2,
    CI = F,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Incidence",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

library(patchwork)

plotbig1 <- p1+p11+plot_layout(ncol=2)+plot_annotation(tag_levels="A")


ggsave(plotbig1,filename = "1projection_incidence.pdf",width = 12,height = 8,dpi = 300,units = "in")
##############
p2 <-  ggprediction_ASR(
    data = bapc_results2,
    CI = T,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Prevalence",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

p21 <-  ggprediction_number(
    data = bapc_results2,
    CI = F,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Prevalence",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

plotbig2 <- p2+p21+plot_layout(ncol=2)+plot_annotation(tag_levels="A")
ggsave(plotbig2,filename = "2projection_Prevalence.pdf",width = 12,height = 8,dpi = 300,units = "in")



##############
p3 <-  ggprediction_ASR(
    data = bapc_results2,
    CI = T,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "DALYs (Disability-Adjusted Life Years)",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

p31 <-  ggprediction_number(
    data = bapc_results2,
    CI = F,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "DALYs (Disability-Adjusted Life Years)",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

plotbig3 <- p3+p31+plot_layout(ncol=2)+plot_annotation(tag_levels="A")

ggsave(plotbig3,filename = "3projection_DALYs.pdf",width = 12,height = 8,dpi = 300,units = "in")

##############
p4 <-  ggprediction_ASR(
    data = bapc_results2,
    CI = T,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Deaths",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

p41 <-  ggprediction_number(
    data = bapc_results2,
    CI = F,
    predict_start = 2024,
    group_name = "location",
    location_name = c("China","United States of America","Japan","Republic of Korea"),
    measure_name = "Deaths",#c("Incidence","Prevalence","DALYs (Disability-Adjusted Life Years)","Deaths"), 
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

plotbig4 <- p4+p41+plot_layout(ncol=2)+plot_annotation(tag_levels="A")

ggsave(plotbig4,filename = "4projection_Deaths.pdf",width = 12,height = 8,dpi = 300,units = "in")




##PAF####



# 中国 vs 美国 青年/非青年卒中的GBD负担和危险因素分析比较
library(tidyverse)
library(easyGBDR)
GBD_edition(edition = 2023)



df1
#1risk factor 数据读取--
datafactor<- GBDread(folder=T,foldername ="./riskdata/2023riskdata")|> 
    filter( age %in% c("0 to 4","5 to 9", "10 to 14",
                       "15 to 19", "20 to 24", "25 to 29", 
                       "30 to 34", "35 to 39", "40 to 44", 
                       "45 to 49", "50 to 54", "55 to 59", 
                       "60 to 64", "65 to 69", "70 to 74", 
                       "75 to 79", "80 to 84", 
                       "85 to 89", "90 to 94", "95 plus",
                       "All ages","Age-standardized")) 

rei_result<- GBDrei_age_recal(
    rei_data=datafactor,
    GBDage_recal_result =df1, ## NULL 代表不计算 PAF 数据
    startage=0,
    endage=95,
    CI = T#,parallel = F
)


reidata1 <- rei_result |> filter(measure == "DALYs (Disability-Adjusted Life Years)") |> 
    filter(metric=="Percent")|> filter(year ==2023) |> filter(sex=="Both") |> 
    mutate(year = as.factor(year))
reidata1$location <- factor(reidata1$location,levels = c("China","United States of America",
                                                         "Japan","Republic of Korea"))

unique(reidata1$rei)
rei_order <- reidata1 %>%
    filter(location=="China") |> 
    filter(year == 2023) %>%
    arrange(val) %>%
    pull(rei) %>%
    unique()

reidata1$rei <- factor(reidata1$rei, levels = rev(rei_order))




ggplot(reidata1,aes(x=rei,y=val,fill =location))+
    geom_col(position = position_dodge(width = 0.7), width = 0.7) +
    scale_fill_jama()+scale_color_jama() +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 80, hjust = 1))+
    labs(
        title = "",
        x = "Risk factors",
        y = "PAF(%)"  # 修改Y轴标签
    ) 


# PAFs of ischemic stroke DALYs due to evaluated risk factors in 2023，Both sex,≥0 years old



level2riskfactorDALYs2023


ggsave(filename = "6level2riskfactorDALYs2023.pdf", width = 10, height =8, units = "in", dpi = 300)










