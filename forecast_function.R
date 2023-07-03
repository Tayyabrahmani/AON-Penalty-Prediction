forecastOutput <- function(model, database){
  probs <- setNames(rep(0, 6), c("prob1", "prob2", "prob3", "prob4", "prob5", "prob6"))

  for(i in names(model$estimate)){
    if (i == "b_asc_TL"){
    probs["prob1"] = probs["prob1"] + model$estimate[i]
    }
    else if(substrRight(i, 1) == "2" || i == "b_asc_TC"){
      if (i == "b_asc_TC"){
        probs["prob2"] = probs["prob2"] + model$estimate[i]
      }
      else {colname = sub("2", "", (sub("b_", "", names(model$estimate[i]))))
      calc_prob = sum(model$estimate[i] * database[colname])
      probs["prob2"] = probs["prob2"] + calc_prob
      }
    }
    else if(substrRight(i, 1) == "3" || i == "b_asc_TR"){
      if (i == "b_asc_TR"){
        probs["prob3"] = probs["prob3"] + model$estimate[i]
      }
      else {
        colname = sub("3", "", (sub("b_", "", names(model$estimate[i]))))
        calc_prob = sum(model$estimate[i] * database[colname])
        probs["prob3"] = probs["prob3"] + calc_prob}
    }
    else if(substrRight(i, 1) == "4" || i == "b_asc_BL"){
      if (i == "b_asc_BL"){
        probs["prob4"] = probs["prob4"] + model$estimate[i]
      }
      else{      
        colname = sub("4", "", (sub("b_", "", names(model$estimate[i]))))
        calc_prob = sum(model$estimate[i] * database[colname])
        probs["prob4"] = probs["prob4"] + calc_prob
      }
    }
    else if(substrRight(i, 1) == "5" || i == "b_asc_BC"){
      if (i == "b_asc_BC"){
        probs["prob5"] = probs["prob5"] + model$estimate[i]
      }
      else{
        colname = sub("5", "", (sub("b_", "", names(model$estimate[i]))))
        calc_prob = sum(model$estimate[i] * database[colname])
        probs["prob5"] = probs["prob5"] + calc_prob
      }
    }
    else if(substrRight(i, 1) == "6" || i == "b_asc_BR"){
      if (i == "b_asc_BR"){
        probs["prob6"] = probs["prob6"] + model$estimate[i]
      }
      else{
        colname = sub("6", "", (sub("b_", "", names(model$estimate[i]))))
        calc_prob = sum(model$estimate[i] * database[colname])
        probs["prob6"] = probs["prob6"] + calc_prob
      }
    }
  }
  sum_prob = sum(sapply(probs[c("prob1", "prob2", "prob3", "prob4", "prob5", "prob6")], exp))
  prob_map = c('prob1'= 'TL', 'prob2'= 'TC', 'prob3'= 'TR', 'prob4'= 'BL', 'prob5'= 'BC', 'prob6'= 'BR')
  return (prob_map[names(which.max(exp(probs) / sum_prob))])
}
