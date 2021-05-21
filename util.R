library(readxl)
library(data.table)
library(ggplot2)
library(lubridate)
library(binom)
library(stringr)
library(lubridate)
library(readstata13)
library(stringi)
library(cowplot)
library(scales)
library(plyr)
library(Hmisc)
library(dplyr)
library(tidyr)
Sys.setenv(TZ='America/Detroit')
library(devtools)



#################################
####### Study 1 functions #######
#################################
get_cint <- function(d,colname="sel"){
  ci <- binom.confint(sum(d[,get(colname)]),nrow(d), methods="agresti-coull")
  return(list(mean=ci$mean,lower=ci$lower,upper=ci$upper))
}

gen_association_model_results <- function(survey_data,scenario_details){
  # get the columns corresponding to this scenario's questions
  assoc <- survey_data[,scenario_details$question_name,with=F]
  
  
  #melt all the answers from all survey respondents into a single format
  melted_assoc <- melt(assoc, measure=names(assoc),variable.factor = F)
  melted_assoc[, value := sub("^a[n]? ","",value)]
  melted_assoc[, value := sub("</span","",value)]
  melted_assoc <- merge(melted_assoc, scenario_details, by.x="variable",by.y="question_name")
  
  ### This is the part where we're training a model for association, so we're not going to look 
  # at the data points with no associations (the "someone" conditions)
  
  modeling_data <- melted_assoc[actor != "someone"]
  
  # (x-y) -> form the "data" matrix; 
  x <- t(apply(modeling_data,1,
               function(m){
                 z <- rep(0,4)
                 z[as.integer(m['i_a_id'])] <- 1
                 z[as.integer(m['i_b_id'])] <- -1
                 return(z)
               }))
  
  # set up the actual answer they gave - 1 for the first identity, i_a, 0 for the second, i_b
  y <- modeling_data$value == modeling_data$i_a
  
  # Okay, now that we have this,we can train a model to learn the "edge weights"
  mod <- glm(y~x-1,family=binomial(link='logit'))
  
  #Add predictions from the model and the actual answers back into the respondent data
  modeling_data[, sel := y]
  modeling_data[, pred := predict(mod,type="response")]
  
  # Combine with the non-model-able data
  nonmodeled_data <- melted_assoc[actor == 'someone']
  # by definition, the prediction is .5
  nonmodeled_data[, pred := .5]
  nonmodeled_data[, sel := nonmodeled_data$value == nonmodeled_data$i_a]
  
  final_data <- rbind(modeling_data,nonmodeled_data)
  
  # Compute the confidence intervals for the real data and add in the predictions
  res <- final_data[,c(list(prediction_pcs_est=mean(pred)),get_cint(.SD)), by=.(variable)]
  
  
  # Return the results!
  return(list('model'=mod, 'results'=res))
}

gen_act_model_results <- function(survey_data,details){
  
  #melt all the answers from all survey respondents into a single format
  melted<- melt(survey_data[,details$question_name,with=F], 
                       measure=details$question_name,
                       variable.factor = F)
  melted[, value := sub("^a[n]? ","",value)]
  melted[, value := sub("</span","",value)]
  melted <- merge(melted, details, by.x="variable",by.y="question_name")
  melted[, y := value == i_a]
  # Okay, now that we have this,we can train a model to learn the "edge weights"
  mod <- glm(y~i_b_deflection-i_a_deflection-1,family=binomial(link='logit'),melted)
  
  #Add predictions from the model and the actual answers back into the respondent data
  melted[, pred := predict(mod,type="response")]
  
  # Compute the confidence intervals for the real data and add in the predictions
  res <- melted[,list(prediction_actweighted=mean(pred)), by=.(variable)]
  
  # Return the results!
  return(list('model'=mod, 'results'=res))
}



gen_trait_model_results <- function(survey_data,details){
  # get the columns corresponding to this scenario's questions
  trait <- survey_data[,details$question_name,with=F]
  
  #melt all the answers from all survey respondents into a single format
  melted_trait <- melt(trait, measure=names(trait),variable.factor = F)
  melted_trait[, value := sub("^a[n]? ","",value)]
  melted_trait[, value := sub("</span","",value)]
  melted_trait <- merge(melted_trait, details, by.x="variable",by.y="question_name")
  melted_trait[, y := value == i_a]
  melted_trait
  # Okay, now that we have this,we can train a model to learn the "edge weights"
  mod <- glm(y~trait_diff,family=binomial(link='logit'),melted_trait)
  
  #Add predictions from the model and the actual answers back into the respondent data
  melted_trait[, pred := predict(mod,type="response")]
  
  # Compute the confidence intervals for the real data and add in the predictions
  res <- melted_trait[,list(prediction_pcs_est=mean(pred)), by=.(variable)]
  
  # Return the results!
  return(list('model'=mod, 'results'=res))
}

load_epa_data <- function(filename,survey_data){
  epa_data <- fread(filename)
  setnames(epa_data, c("term","e","p","a"))
  
  # add soccer coach and soccer player as coach and player, respectively
  epa_data <- rbind(epa_data,
                    data.table(term=c("soccer coach", "soccer player"), 
                               epa_data[term %in% c('coach',"player"),.(e,p,a)]))
  
  # add "someone" as well
  someone_df <- data.table(term="someone", 
                           e=mean(as.numeric(survey_data$someone_e_1)),
                           p=mean(as.numeric(survey_data$someone_p_1)),
                           a=mean(as.numeric(survey_data$someone_a_1)))
  epa_data <- rbind(epa_data,someone_df)
  return(epa_data)
}


gen_comb_predictions <- function(survey_data,details, comb_diff_col="cult_dist"){
  model_data <- survey_data[,details$question_name,with=F]
  
  #melt all the answers from all survey respondents into a single format
  melted_model_data <- melt(model_data, measure=names(model_data),variable.factor = F)
  melted_model_data[, value := sub("^a[n]? ","",value)]
  melted_model_data[, value := sub("</span","",value)]
  melted_model_data <- merge(melted_model_data, details, by.x="variable",by.y="question_name")
  
  melted_model_data[, defl_diff := i_b_deflection - i_a_deflection]
  # set up the actual answer they gave - 1 for the first identity, i_a, 0 for the second, i_b
  melted_model_data[, y := value == i_a]
  
  # Okay, now that we have this,we can train a model to learn the "edge weights"
  mod <- glm(formula(paste0("y~-1+defl_diff+",comb_diff_col)),family=binomial(link='logit'),data=melted_model_data)
  
  #Add predictions from the model and the actual answers back into the respondent data
  melted_model_data[, sel := y]
  melted_model_data[, pred := predict(mod,type="response")]
  
  # Compute the confidence intervals for the real data and add in the predictions
  res <- melted_model_data[,list(prediction_comb=mean(pred)), by=.(variable)]
  return(list('model'=mod,'results'=res))
}

get_name_data <- function(suffix, names){
  questions <- paste0(names,suffix)
  res <- melt(survey_data[,questions,with=F],measure.vars = questions)
  res[,variable := sub(suffix,"",variable)]
  res[,value := as.numeric(value)]
  return(res)
}

fill_datatable_na_with_zero <- function(DT){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = 0)
  }
}



error_plot <- function(dat,varname){
  data <- melt(dat, 
               id=c("name",varname,"mean","lower","upper","affect_cond"),
               measure=c("prediction_affect","prediction_pcs_est", "prediction_comb"))
  setnames(data, varname, "cond")
  data$variable <- factor(data$variable, 
                          levels=c("prediction_affect","prediction_comb","prediction_pcs_est"),
                          labels=c("ACT","LCSS","PCS-FA"))
  
  data$m <- data$mean - data$value
  data$l <- data$lower - data$value
  data$u <- data$upper - data$value
  
  p<- ggplot(data, aes(x=variable,y=m,ymin=l,ymax=u,group=name)) 
  p <- p + geom_pointrange() + geom_hline(yintercept = 0,color='red') + geom_line()
  p <- p + scale_y_continuous("Expected Mean Average Error") + xlab("Model")
  p + facet_grid(cond~affect_cond,scales="free_y")
}


full_res_plot <- function(data,cond_name){
  pltdat <- melt(data, id=c("name","affect_cond",cond_name),
                 measure=c("mean","prediction_affect","prediction_pcs_est","prediction_comb","prediction_pcshc"))
  setnames(pltdat, cond_name, "cond")
  #,"comb Prediction","prediction_comb"
  pltdat[,Model := mapvalues(variable,
                             c("mean",
                               "prediction_affect",
                               "prediction_pcs_est",
                               "prediction_comb",
                               "prediction_pcshc"),
                             c("Empirical\nMean",
                               "ACT\nPrediction",
                               "Estimated\nPCS-FA\nPrediction",
                               "LCSS\nPrediction",
                               "Hand-coded\nPCS-FA\nPrediction"))]
  pltdat <- pltdat[order(-affect_cond,cond)]
  pltdat$order <- 1:nrow(pltdat)
  
  full_plt <- ggplot(pltdat, aes(reorder(name,order),
                                 value,
                                 fill=Model))
  full_plt <- full_plt + geom_line(aes(group=name,color=affect_cond),size=1.2) 
  full_plt <- full_plt + geom_point(size=ifelse(pltdat$Model == "Hand-coded\nPCS-FA\nPrediction",3,
                                                ifelse(pltdat$Model == "Empirical\nMean",7,5)),
                                    shape=ifelse(pltdat$Model == "Empirical\nMean",23,21),
                                    stroke=0)  + coord_flip() 
  full_plt <- full_plt + xlab("") + scale_y_continuous("% of Respondents",labels=percent)
  full_plt <- full_plt + scale_fill_manual("Estimate from", 
                                           values=c("black",'purple',"orange","blue", "grey"))
  full_plt <- full_plt + theme(legend.key.size = unit(3.5, 'lines'),
                               legend.background = element_rect(color='black',size=.5, linetype="dashed"))
  full_plt <- full_plt + scale_shape_discrete(guide=F)
  full_plt <- full_plt +  guides( fill = guide_legend(title.position="top",
                                                      ncol=1,override.aes = list(shape = c(23,21,21,21,21),
                                                                                 size=c(12,10,10,10,8))),
                                  col= guide_legend(title.position="top",ncol=1,override.aes = list(size=2.5)))
  full_plt <- full_plt + facet_grid(cond~., space="free_y",scales="free_y")
  full_plt <- full_plt + scale_color_manual("Affecting\nDeflection\nCondition", values=c("Green","Red"))
  full_plt
}

