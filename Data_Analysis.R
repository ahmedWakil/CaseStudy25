#########################SECTION 1: TABLE OF VARIABLES#########################
library(readxl)
library(formattable)
variable_table = read_excel("/Users/shivi/Documents/School/STAC51/Case Study/variable_meanings.xlsx", 
                  col_names = TRUE, skip = 1)

variable_table[is.na(variable_table)]=''
formattable(variable_table, align = c(rep("c",5)), 
            list('Type of Variable' = formatter
                 ("span", style = ~ style(font.weight = "bold"))))

########################SECTION 2: PRELIM DATA ANALYSIS########################
#reading the data
library(readxl)
data = read_excel("/Users/shivi/Documents/School/STAC51/Case Study/BrownFat.xls")

#modifying the classes of the variables to allow for analysis
data$Sex = as.factor(data$Sex)
data$Day = as.factor(data$Day)
data$Month = as.factor(data$Month)
data$Season = as.factor(data$Season)
data$Diabetes = as.factor(data$Diabetes)
data$Cancer_Status = as.factor(data$Cancer_Status)
data$Cancer_Type = as.factor(data$Cancer_Type)
data$BrownFat = as.factor(data$BrownFat)
data$TSH = as.numeric(data$TSH)
data$Total_vol = as.numeric(data$Total_vol)
data = as.data.frame(data)

#fitting logistic regression models, one for BrownFat and one for Total_vol
presence_of_brownfat = glm(BrownFat ~ Sex + Age + Size + Weigth + BMI + LBW 
                           + Day + Month + Season + Duration_Sunshine + Ext_Temp 
                           + `2D_Temp` + `3D_Temp` + `7D_Temp` + `1M_Temp` 
                           + Diabetes  + Glycemy  + Cancer_Status 
                           + Cancer_Type, family = binomial, data = data)


#checking variable significance
options(max.print=999999)
summary(presence_of_brownfat)
drop1(presence_of_brownfat, test = "Chisq")

#########################SECTION 3: DATA VISUALIZATION#########################
#contingency table for Sex
sex_cont_table = table(data$BrownFat, data$Sex)
OR_sex = sex_cont_table[1,1]*sex_cont_table[2,2]/(sex_cont_table[1,2]*sex_cont_table[2,1])

#contingency table for Diabetes
diabetes_cont_table = table(data$BrownFat, data$Diabetes)
OR_diabetes = diabetes_cont_table[1,1]*diabetes_cont_table[2,2]/
  (diabetes_cont_table[1,2]*diabetes_cont_table[2,1])

#making graphs for visualization
library(ggplot2)
#visualizing Sex
ggplot(data, aes(x = Sex, fill = BrownFat)) + geom_bar(position = "dodge") + scale_x_discrete(labels = c("Female", "Male")) + labs(y = "Patients") + scale_fill_discrete(name = "Brown Fat", labels = c("Absent", "Present")) + ggtitle("Figure 1: Sex and Brown Fat")

#Visualizing Age
ggplot(data, aes(x = Age, y = BrownFat)) + geom_boxplot() + labs(y = "Brown Fat") + scale_y_discrete(labels = c("Absent", "Present")) + ggtitle("Figure 2: Age and Brown Fat")

##visualizing Diabetes
ggplot(data, aes(x = Diabetes, fill = BrownFat)) + geom_bar(position = "dodge") + scale_x_discrete(labels = c("Absent", "Present")) + labs(y = "Patients") + scale_fill_discrete(name = "Brown Fat", labels = c("Absent", "Present")) + ggtitle("Figure 3: Diabetes and Brown Fat")