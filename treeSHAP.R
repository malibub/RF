
library(treeshap)
#### shapley ----


model_unified <- ranger.unify(model, train)
mean(train$HeartDiseaseorAttack) # this is the baseline value
# predictions for P(Y=1| X) on the mini_data :
mini_data_pred = predict(model, test, type = "response")$predictions[,2]
mean(mini_data_pred[test$HeartDiseaseorAttack == 1]) # mean pred for class 1 cases
mean(mini_data_pred[test$HeartDiseaseorAttack == 0]) # mean pred for class 0 cases

# BEESWARM
# https://cran.r-project.org/web/packages/shapviz/vignettes/basic_use.html
shaps <- treeshap(model_unified, test, interactions = TRUE)

shp <- shapviz(shaps, X = test)
sv_importance(shp)
sv_importance(shp, kind = "beeswarm")
sv_importance(shp, kind = "both", show_numbers = TRUE, bee_width = 0.2)


plot_feature_dependence(shaps, variable = "Age")
plot_feature_dependence(shaps, variable = "Sex")
plot_feature_dependence(shaps, variable = "BMI")
plot_feature_dependence(shaps, variable = "HighBP")

#unified2 <- set_reference_dataset(model_unified, mini_data[1:200, ])
obs_id = 9
treeshap_res <- treeshap(model_unified, mini_data[obs_id, ])
print(predict(rf, mini_data[obs_id,], type = "response")$predictions)
treeshap_res$shaps[1,]
treeshap_res$observations[1,]
print(plot_contribution(treeshap_res
                        , min_max = c(0, 1))) # in the plots factor variables are are displayed with a +1
for (obs_num in 1:3) {
  print(plot_contribution(treeshap_res, obs = obs_num
                          , min_max = c(0, 1))
  )
}
plot_feature_importance(treeshap_res, max_vars = 8)


inter <- treeshap(model_unified, mini_data[1:200,], interactions = T)
plot_interaction(inter, 'Age', 'Sex')
