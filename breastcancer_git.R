library(tidyverse)
library(caTools)
suppressWarnings(library(ROCR))
suppressWarnings(library(pROC))
library(magrittr)
library(dplyr)
library(dlookr)
library(caret)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)
#Breast Cancer Wisconsin Data Analysis

# import dataset
bc_win <- read.csv("breastcancer.csv")

head(bc_win)

map_int(bc_win, function(.x) sum(is.na(.x)))

bc_win %>% 
  dim()
bc_win %>%
  str()
#
bc_win %>%
  count(diagnosis) %>%
  group_by(diagnosis) %>%
  summarize(perc_dx = round((n/569)*100, 2))

summary(bc_win)

#pseudorandom numbers
set.seed(42)

# splitting data 75/25 train/test split
split_data <- sample.split(Y = bc_win$diagnosis, SplitRatio = 0.75)
training <- subset(x = bc_win, split_data == TRUE)
testing <- subset(x = bc_win, split_data == FALSE)


#diagnosis column possible labels M (malignant) and B (benign)
model_bc <- rpart(diagnosis ~ ., data = training, method = "class")

# descriptive output for model
print(model_bc)
# tree visualisation
rpart.plot(model_bc)

fdiag <- factor(testing$diagnosis)
fpred <- factor(preds_bc, levels = levels(fdiag))

# test dataset predictions
preds_bc <- predict(model_bc, newdata = testing, type = "class")
print(preds_bc)

#confusion matrix 
cm <- confusionMatrix(fpred, fdiag)
print(cm)
conf_m <- as.data.frame(cm$table)

#plot using ggplot
ggplot(data = conf_m, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  ggtitle("Confusion Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
