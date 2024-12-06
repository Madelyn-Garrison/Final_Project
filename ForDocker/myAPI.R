#myAPI.R 
library(GGally)
library(leaflet)
library(tidyverse)
library(tidymodels)


my_diabetes_data<-read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

my_data<-read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")|>
  mutate(Fruits=as.factor(ifelse(Fruits==0, "No fruit", "Fruit")),
         Stroke=as.factor(ifelse(Stroke==0, "No stroke", "Stroke")),
         Smoker=as.factor(ifelse(Smoker==0, "No 100 cigs", "100 cigs")),
         HeartDiseaseorAttack=as.factor(ifelse(HeartDiseaseorAttack==0, "No CHD/MI", "CHD/MI")),
         PhysActivity=as.factor(ifelse(PhysActivity==0, "No physical activity past 30 days", "Physical activity past 30 days")),
         CholCheck=as.factor(ifelse(CholCheck==0, "No cholesterol check in 5 years", "Cholesterol check in past 5 years")),
         HighChol=as.factor(ifelse(HighChol==0, "No high cholesterol", "High cholesterol")),
         HighBP=as.factor(ifelse(HighBP==0, "No high blood pressure", "High blood pressure")),
         Diabetes_binary=as.factor(Diabetes_binary),
         Veggies=as.factor(ifelse(Veggies==0, "No veggies", "Veggies")),
         HvyAlcoholConsump=as.factor(ifelse(HvyAlcoholConsump==0, "Not heavy Drinker", "Heavy Drinker")),
         AnyHealthcare=as.factor(ifelse(AnyHealthcare==0, "No healthcare", "Healthcare")),
         GenHlth=as.factor(ifelse(GenHlth==1, "Excellent", ifelse(GenHlth==2, "Very good", ifelse(GenHlth==3, "Good", ifelse(GenHlth==4, "Fair", "Poor"))))),
         DiffWalk=as.factor(ifelse(DiffWalk==0, "Can walk", "Difficult walking")),
         Sex=as.factor(ifelse(Sex==0, "female", "male")),
         Age=as.factor(Age),
         Education=as.factor(Education),
         Income=as.factor(Income)
  )

set.seed(15)
data_split <- initial_split(my_data, prop = 0.70)
data_train <- training(data_split)
data_test <- testing(data_split)
data_5_fold <- vfold_cv(data_train, 5)

tree_rec_2 <- recipe(Diabetes_binary ~ HighBP + HighChol + BMI + AnyHealthcare + Age + GenHlth, data = data_train) |>
  step_dummy(HighBP,HighChol,AnyHealthcare,Age,GenHlth)
LR_spec <- logistic_reg() |>
  set_engine("glm")
LR2_wkf <- workflow() |>
  add_recipe(tree_rec_2) |>
  add_model(LR_spec)

#* Find multiple of two numbers
#* @param num1 HighBP: No high blood pressure/High blood pressure
#* @param num2 HighChol: No high cholesterol/High cholesterol
#* @param num3 BMI: Number
#* @param num4 AnyHealthcare: No healthcare/Healthcare
#* @param num5 GenHlth: Excellent/Very good/Good/Fair/Poor
#* @param num6 Age: 1-13
#* @get /pred
function(num1, num2, num3, num4, num5, num6){
  x<-last_fit(LR2_wkf, data_split)
  xx<-x %>% extract_workflow()
  xxx<-data.frame(cbind(num1,num2,num3,num4,num5,num6))
  colnames(xxx)<-colnames(my_data[c(2,3,5,13,15,20)])
  xxx<-xxx|>mutate(HighBP=as.factor(HighBP), HighChol=as.factor(HighChol), BMI=as.double(BMI),
                   AnyHealthcare=as.factor(AnyHealthcare), GenHlth=as.factor(GenHlth), Age=as.factor(Age))
  predict(xx, xxx)
}

# High blood pressure ---- No high cholesterol ------ 40 ------- No healthcare -------- Poor -------- 8
# High blood pressure ---- High cholesterol ------ 70 ------- Healthcare -------- Fair -------- 7
# No igh blood pressure ---- No high cholesterol ------ 50 ------- Healthcare -------- Good -------- 6

#* @get /info
function(){
  "Madelyn Garrison https://github.com/Madelyn-Garrison/Final_Project"
}

#* @serializer png
#* @get /confusion
function(){
  LR_train_fit <- LR2_wkf |>
    fit(my_data)
  t<-conf_mat(my_data |> mutate(estimate = LR_train_fit |> predict(my_data) |> pull()), 
           Diabetes_binary, 
           estimate)
  tt<-autoplot(t, type = "heatmap")
  print(tt)
}
