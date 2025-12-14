setwd('results')

df <- GBDread(folder=T,foldername ='data')
write.csv(df,'2023stroke.csv')


##年龄0-95+
df1 <- df |> filter( age %in% c("0 to 4","5 to 9", "10 to 14",
                                "15 to 19", "20 to 24", "25 to 29", 
                                "30 to 34", "35 to 39", "40 to 44", 
                                "45 to 49", "50 to 54", "55 to 59", 
                                "60 to 64", "65 to 69", "70 to 74", 
                                "75 to 79", "80 to 84", 
                                "85 to 89", "90 to 94", "95 plus",
                                "All ages","Age-standardized")) 

##1.读取数据：####
data=debug_gbd_data_check(df)
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

##EAPC####
EAPC <- GBDeapc(data=df1 , 
                      rei = F, 
                      EAPC_95CI = T,
                      digits = 2, 
                      sep = " to ")  
write.csv(EAPC,"EAPC.csv")

library(dplyr)
measure_order <- c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)","Deaths")

EAPCplotdata <- EAPC |> 
    filter(age %in% c("Age-standardized")) |>
    arrange(match(location, measure_order))

EAPCplotdata$measure <- factor(EAPCplotdata$measure, levels = measure_order)

ggplot(EAPCplotdata, aes(x = location, y = EAPC, fill = sex)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(
        aes(ymin = LCI, ymax = UCI),
        position = position_dodge(0.8),
        width = 0.25
    ) +
    facet_wrap(~ measure, nrow = 2) +  
    scale_fill_jama()+scale_color_jama() +theme_bw()+
    labs(
        x = "Location",
        y = "EAPC Value",
        fill = "Sex"
    ) +
    theme(
        legend.position = "top",
        axis.text.x = element_text(#angle = 45, 
            hjust = 1))
ggsave("./202512正式分析结果/EAPCplot.pdf",width = 10,height = 8,dpi = 300,units = "in")


##年龄变化趋势--------####

plot1data <- df1 |> 
    filter(year == 2023, metric == "Rate") |> 
    filter(sex %in% c("Female", "Male"))|> filter(!age %in%c("All ages","Age-standardized"))


age_order <- c("95 plus","90 to 94","85 to 89","80 to 84","75 to 79","70 to 74",
               "65 to 69","60 to 64","55 to 59","50 to 54","45 to 49","40 to 44",
               "35 to 39","30 to 34","25 to 29","20 to 24","15 to 19","10 to 14",
               "5 to 9","0 to 4")
  
plot1data <- plot1data %>%
    mutate(val = ifelse(sex == "Female", -val, val))

plot1data$measure <- factor(plot1data$measure, levels = measure_order)
plot1data$age <-  factor(plot1data$age, levels = rev(age_order))

ggplot(plot1data, aes(x = val, y = age, fill = location)) + 
    geom_col(position = "stack", orientation = "y") + 
    facet_wrap(~ measure, nrow = 2, scales = "free_x") + 
    scale_fill_jama() + scale_color_jama() +
        annotate("text", x = Inf, y = Inf, label = "Male", 
             hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
    annotate("text", x = -Inf, y = Inf, label = "Female", 
             hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
    labs(x = "Rate per 100,000 population", 
         y = "Age",
         fill = "Location") +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 0, hjust = 1))

ggsave("2.1ageTrend.pdf",width = 10,height = 8,dpi = 300,units = "in")

write.csv(plot1data,"agetrenddata.csv")

##年份变化趋势######

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
        y = "ASR per 100,000 population",
        colour = "Country"
    ) +
    theme_bw() +
    theme(
        legend.position = "top",
        strip.background = element_rect(fill = "lightgray"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.7),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.8, "lines")
    )


ggsave("3YearTrend.pdf",width = 10,height = 8,dpi = 300,units = "in")


write.csv(plot3data,"yeartrenddata.csv")



##APC####



config_stata("D:/Stata17", version = 17, stata_type = c("MP"))

library(tidyverse)
apc1 <- GBDapc_interact(df1,
                        startyear = 1994,
                        endyear = 2023)


table1 <- apcinteract_table(
    data= apc1,
    type = "age_period",
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",#
    rei_name = NULL,
    sex_name = "Both"
)

write.csv(table1, file = "APC_age_period.csv")


table2 <- apcinteract_table(
    data= apc1,
    type = "age_cohort",
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)

write.csv(table2, file = "APC_age_cohort.csv")

table3 <- apcinteract_table(
    data= apc1,
    type = "period_cohort",
    CI = T,
    digits = 2,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = c("Incidence", "Prevalence", "DALYs (Disability-Adjusted Life Years)", "Deaths"),
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)

write.csv(table3, file = "APC_period_cohort.csv")


#3)

ggapc_interaction(
    data = apc1,
    type = c("age_period"),
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = "DALYs (Disability-Adjusted Life Years)",
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = c("Both"),
    line_size = 1 
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.1DALYS_age_period.pdf", width = 10, height =8, units = "in", dpi = 300)

#####
ggapc_interaction(
    data = apc1,
    type = c("age_cohort"),
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = "DALYs (Disability-Adjusted Life Years)",
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = c("Both"),
    line_size = 1 
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.2DALYS_age_cohort.pdf", width = 10, height =8, units = "in", dpi = 300)

######
ggapc_interaction(
    data = apc1,
    type = c("period_cohort"),
    CI = F,
    location_name = c("China","United States of America",
                      "Japan","Republic of Korea"),
    measure_name = "DALYs (Disability-Adjusted Life Years)",
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = c("Both"),
    line_size = 1 
) + facet_wrap(.~location, scales = "free_y") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "5.3DALYS_period_cohort.pdf", width = 10, height =8, units = "in", dpi = 300)


###分解分析####

names(df1)
measure_order <- measure_order 
temp <- df1|>filter(sex=="Both")
temp$measure <- factor(temp$measure, levels = measure_order)


min(temp$val)

# 构建模型
decomposition_result <- GBDdecomposition(
    data=temp, 
    byear = 1990,
    compareyear = 2023, 
    startage = 0,
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

library(writexl)

write_xlsx(decom_table, "decomposition_results.xlsx")


ggdecomposition(
    data=decomposition_result,
    measure_name=c("Incidence","Prevalence","Deaths",
                   "DALYs (Disability-Adjusted Life Years)"),
    location_name=unique(temp$location),
    sex_name=unique(temp$sex),
    rei_name = NULL,
    cause_name=unique(temp$cause),
    percent = F  
)+ theme_bw()+facet_wrap(~measure,ncol=2,scales = "free_x")+
    scale_fill_jama()+scale_color_jama() 

ggsave("4decomposition.pdf",width = 10,height = 8,dpi = 300,units = "in")





##BAPC########

options(timeout = 6000)


library(tidyverse)
library(easyGBDR)
library(dplyr)
library(ggsci)
library(patchwork)
library(ggplot2)
library(fmesher)
library(future.apply)
library(pbapply)
library(parallel)
library(doParallel)
library(data.table)

GBD_edition(edition = 2023)

plot3data<-  df1

measure_order <-measure_order

plot3data$measure <- factor(plot3data$measure, levels = measure_order)


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
    measure_name = "Incidence",
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
    measure_name = "Incidence",
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
    measure_name = "Prevalence",
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
    measure_name = "Prevalence",
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
    measure_name = "DALYs (Disability-Adjusted Life Years)",
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
    measure_name = "DALYs (Disability-Adjusted Life Years)",
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
    measure_name = "Deaths",
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
    measure_name = "Deaths",
    cause_name = "Ischemic stroke",
    rei_name = NULL,
    sex_name = "Both"
)+ theme_bw() +scale_fill_jama() + scale_color_jama() +theme(legend.position = "top")+
    facet_wrap(~location,ncol = 2,, scales = "free_y")

plotbig4 <- p4+p41+plot_layout(ncol=2)+plot_annotation(tag_levels="A")

ggsave(plotbig4,filename = "4projection_Deaths.pdf",width = 12,height = 8,dpi = 300,units = "in")




##PAF####

library(tidyverse)
library(easyGBDR)
GBD_edition(edition = 2023)



df1

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
    GBDage_recal_result =df1, 
    startage=0,
    endage=95,
    CI = T)

reidata1 <- rei_result |> filter(measure == "DALYs (Disability-Adjusted Life Years)") |> 
    filter(metric=="Percent")|> filter(year ==2023) |> filter(sex=="Both") |> 
    mutate(year = as.factor(year))
reidata1$location <- factor(reidata1$location,levels = c("China","United States of America",
                                                         "Japan","Republic of Korea"))

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
        y = "PAF(%)" ) 

level2riskfactorDALYs2023

ggsave(filename = "6level2riskfactorDALYs2023.pdf", width = 10, height =8, units = "in", dpi = 300)
