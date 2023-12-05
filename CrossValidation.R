cat("Current working directoy:",getwd())
### install pacages ----
# https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset?select=heart_disease_health_indicators_BRFSS2015.csv

# for implementation of CART with eneabled cost specification:
# https://cran.r-project.org/web/packages/rpart/rpart.pdf
library(readr)
library(ranger)
library(pROC)
library(treeshap)
library(shapviz)
library(caret) # for cross validation


# loval imports 
#source(paste(getwd(),"/macros/treeSHAP_unify_ProbabilityTree_modified.R", sep = ""))
source("macros/treeSHAP_unify_ProbabilityTree_modified.R")

set.seed(27021998)

#### data prep ----
heart_disease_df <- read_csv("data/heart_disease_health_indicators_BRFSS2015.csv"
                             ,  col_types = cols(HeartDiseaseorAttack = col_double()))
# View(heart_disease_df)  
factor_vars <- c(#"HeartDiseaseorAttack"
  "HighBP"
  , "HighChol"
  #, "CholCheck"
  #, "Smoker"
  #, "Stroke"
  #, "Diabetes"
  #, "PhysActivity"        
  #, "Fruits"               
  #, "Veggies"              
  , "HvyAlcoholConsump"   
  #, "AnyHealthcare"                   
  #,"DiffWalk"            
  , "Sex"                  
  #, "Education" 
)
heart_disease_df[factor_vars] <- lapply(heart_disease_df[factor_vars], function(var) as.factor(var))

# divide dataset into test- and trainingset
size_testset <- 0.2
#heart_disease_df$flag_trainset <- as.factor(runif(dim(heart_disease_df)[1]) > size_testset)
heart_disease_df$random_number <- runif(dim(heart_disease_df)[1])
train <- heart_disease_df[heart_disease_df$random_number > 0.2,]#[, c(-(dim(heart_disease_df)[2]), -(dim(heart_disease_df)[2]-1))]
test <- heart_disease_df[heart_disease_df$random_number <= 0.2,]#[, c(-(dim(heart_disease_df)[2]), -(dim(heart_disease_df)[2]-1))]
# newdata = test[ , !(names(test) %in% c("HeartDiseaseorAttack"))]
# mini_data <- heart_disease_df[heart_disease_df$random_number < 0.001,][, c(-(dim(heart_disease_df)[2]), -(dim(heart_disease_df)[2]-1))]
#mini_data <- heart_disease_df[1:10,][, c(-(dim(heart_disease_df)[2]), -(dim(heart_disease_df)[2]-1))]


data = heart_disease_df
target = "HeartDiseaseorAttack"

# i = 1
k = 4
folds <- createFolds(heart_disease_df$HeartDiseaseorAttack, k = k, list = TRUE, returnTrain = TRUE)

roc_data <- list()
for (i in 1:k) {
  cat("Iteration", i, "\n")
  
  # Create training and test sets
  train_data <- data[folds[[i]], ]
  test_data <- data[-folds[[i]], ]
  
  cat("Number of training data:", nrow(train_data), "\n")
  cat("Test data count:", nrow(test_data), "\n")
  
  # formular <- paste("as.factor(",target, ") ~ .")
  model <- ranger(
    formula = as.factor(HeartDiseaseorAttack) ~ . # !!! this is necessary for the treeInfo to work for probability trees
    #formula = HeartDiseaseorAttack ~ .
    , data = train_data
    , num.trees = 5
    , mtry = 1
    , importance = "permutation"
    , write.forest = TRUE
    , probability = TRUE
    , min.node.size = 10
    , min.bucket = NULL
    , max.depth = NULL
    , replace = TRUE
    , case.weights = NULL
    , class.weights = NULL
    , scale.permutation.importance = FALSE
    , local.importance = TRUE
    , keep.inbag = FALSE
    , oob.error = TRUE
    , verbose = TRUE
    , classification = TRUE
    , always.split.variables = NULL # variable names to be always selected in addition to the mtry variables
  )
  model
  # Predict using test data
  
  predictions <- predict(model, test_data[ , !(names(test_data) %in% c("HeartDiseaseorAttack"))])$predictions[,2]
  
  #roc_pred_input <- paste("test_data$",target, sep = "")
  roc_curve <- roc(as.factor(test_data$HeartDiseaseorAttack), predictions)
  roc_auc <- auc(roc_curve)
  roc_data[[i]] <- roc_curve
  
  cat("AUC:", roc_auc, "\n\n")
}



plot(roc_data[[1]], main = "Multiple ROC Curves", col = 1, lwd = 2, cex = 1.5)

# Add other ROC curves to the plot
for (i in 2:length(roc_data)) {
  lines(roc_data[[i]], col = i, lwd = 2)
}

# Add a legend
legend("bottomright", legend = 1:length(roc_data), col = 1:length(roc_data), lwd = 2, title = "Curve")




