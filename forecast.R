dc_seasonal_90s_pred <- data.frame(year = 2022:2032)
data_temp <- dc_seasonal_90s

for(var in names(coef(big_model_dc))[-1]) {
  print(data_temp$var_temp)
  data_temp$var_temp <- dc_seasonal_90s[, var]
  var_temp <- predict(lm(var_temp ~ year, data = data_temp),
                      newdata = data.frame(year = 2021:2031))
  dc_seasonal_90s_pred$var_temp <- var_temp
  colnames(dc_seasonal_90s_pred)[ncol(dc_seasonal_90s_pred)] <- var
}


predictions_vancouver <- tibble(location = 'vancouver',
                                year = 2022:2032) %>% 
  bind_cols(predicted_doy = round(predict(big_model_dc, newdata = vancouver_seasonal_90s_pred)))

predictions_dc <- tibble(location = 'washingtondc',
                                year = 2022:2032) %>% 
  bind_cols(predicted_doy = round(predict(big_model_dc, newdata = dc_seasonal_90s_pred)))

predictions_kyoto <- tibble(location = 'kyoto',
                                year = 2022:2032) %>% 
  bind_cols(predicted_doy = round(predict(big_model_dc, newdata = kyoto_seasonal_90s_pred)))

predictions_liestal <- tibble(location = 'liestal',
                         year = 2022:2032) %>% 
  bind_cols(predicted_doy = round(predict(big_model_dc, newdata = liestal_seasonal_90s_pred)))


predictions <- bind_rows(predictions, predictions_liestal)


submission_predictions <- predictions %>% 
  filter(year > 2021) %>% 
  mutate(predicted_doy = round(predicted_doy)) %>% 
  pivot_wider(names_from = 'location', values_from = 'predicted_doy') %>% 
  select(year, kyoto, liestal, washingtondc, vancouver)

write.csv(submission_predictions, file = "cherry-predictions.csv",
          row.names = FALSE)
