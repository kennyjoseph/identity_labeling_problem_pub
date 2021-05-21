source("util.R")
source("deflection_computation_functions.R")
theme_set(theme_bw(20))

# Get the survey data
survey_data <- fread("./data/study/study1_results.csv")

# Load epa data
epa_data <- load_epa_data(filename = "data/act/clean_epa_terms.txt",
                          survey_data = survey_data)

## get deflection estimates from deflection file
act_eq <- read.csv("data/act/sexes_averaged_1978.txt",header=F,sep="\t", stringsAsFactors=F)
coeff_matrix <- t(as.matrix(act_eq[,c(2:ncol(act_eq))]))
coeff_list <- lapply(sub("Z","",act_eq$V1), function(x){as.integer(str_locate_all(x,"1")[[1]][,1])})

## get modifier estimates from deflection file
modifier_act_eq <- read.csv("data/act/modifier_1978.txt",header=F,sep="\t", stringsAsFactors=F)
modifier_coeff_matrix <- t(as.matrix(modifier_act_eq[,c(2:ncol(modifier_act_eq))]))
modifier_coeff_list <- lapply(sub("Z","",modifier_act_eq$V1), function(x){as.integer(str_locate_all(x,"1")[[1]][,1])})

# Load in the experimental details
experiment_details_trait <-data.table(read_excel("data/study/experiment_details_trait_v_affect.xlsx"))
experiment_details_assoc <-data.table(read_excel("data/study/experiment_details.xlsx"))


#####################################################################
#####################################################################
# Load in association/affect results
#####################################################################
#####################################################################
#####################################################################

# To make the modeling easier
identity_mapping <- list("soccer player" = 1,
                         "sports fan"=2,
                         "shop clerk"=3,
                         "goon"=4,
                         "patient"= 1,
                         "paramedic"=2,
                         "cousin"=3,
                         "trespasser"=4)
identity_map <- data.table(identity=names(identity_mapping),id=as.integer(identity_mapping))

# Cultural association network
assoc_network <- data.table(read_excel("data/study/proximities_association.xlsx"))
assoc_network <- assoc_network[!duplicated(assoc_network)]


# Merge with the numeric identity mapping
experiment_details_assoc <- merge(experiment_details_assoc, identity_map,by.x="i_a",by.y="identity",all.x=T)
setnames(experiment_details_assoc,"id","i_a_id")
experiment_details_assoc <- merge(experiment_details_assoc, identity_map,by.x="i_b",by.y="identity",all.x=T)
setnames(experiment_details_assoc,"id","i_b_id")

# Merge with cultural proximity measures
experiment_details_assoc <- merge(experiment_details_assoc,assoc_network,by.x=c("actor","i_a"),by.y=c("actor","identity"))
setnames(experiment_details_assoc, "proximity","cult_prox_i_a")
experiment_details_assoc <- merge(experiment_details_assoc,assoc_network,by.x=c("actor","i_b"),by.y=c("actor","identity"))
setnames(experiment_details_assoc, "proximity","cult_prox_i_b")

# Merge with ACT data
# get deflection for i_a
i_a_affective <- experiment_details_assoc[,.(actor,behavior,i_a)]
setnames(i_a_affective, "i_a","object")
i_a_affective[, deflection := deflection_computation(coeff_matrix,coeff_list,
                                                     get_epa_for_abo_combination(.SD,epa_data)),by=1:nrow(i_a_affective)]
experiment_details_assoc[, i_a_deflection := i_a_affective$deflection]
# get deflection for i_b
i_b_affective <- experiment_details_assoc[,.(actor,behavior,i_b)]
setnames(i_b_affective, "i_b","object")
i_b_affective[, deflection := deflection_computation(coeff_matrix,coeff_list,
                                                     get_epa_for_abo_combination(.SD,epa_data)),by=1:nrow(i_b_affective)]
experiment_details_assoc[, i_b_deflection := i_b_affective$deflection]

# Get association model results for the association/affect model
s1_results <- gen_association_model_results(survey_data,experiment_details_assoc[scenario == 1])
s2_results <- gen_association_model_results(survey_data,experiment_details_assoc[scenario == 2])
results <- rbind(s1_results[['results']],s2_results[['results']])
summary(s1_results[['model']])
summary(s2_results[['model']])

# Add in a weighted act_prediction
results_act_weighted <- gen_act_model_results(survey_data,experiment_details_assoc)

# Get comb predictions
experiment_details_assoc[, cult_dist := cult_prox_i_a - cult_prox_i_b]
results_comb <- gen_comb_predictions(survey_data,experiment_details_assoc,"cult_dist")
summary(results_comb[['model']])


# Make/merge in predictions
experiment_details_assoc[, prediction_affect := 1/(1+ exp(i_a_deflection-i_b_deflection))]
experiment_details_assoc[, prediction_pcshc := 1/(1+ exp(cult_prox_i_b - cult_prox_i_a))]
experiment_details_assoc <- merge(experiment_details_assoc,results,by.x="question_name",by.y="variable")
experiment_details_assoc <- merge(experiment_details_assoc,results_comb[['results']],by.x="question_name",by.y="variable")
experiment_details_assoc <- merge(experiment_details_assoc,results_act_weighted[['results']],by.x="question_name",by.y="variable")

experiment_details_assoc[, assoc_cond := factor(assoc_cond,levels=c("None","Low (Diff. Institution)","Medium (Institution)","High (Role Pair)"))]
experiment_details_assoc[, name := paste0("Who does a ", actor," ", behavior, ", a\n", i_a, " or a ",i_b,"?")]
experiment_details_assoc$name <- sub("a someone","someone",experiment_details_assoc$name)


#####################################################################
#####################################################################
# Load in trait/affect results
#####################################################################
#####################################################################
#####################################################################


names <- c("johnny","ethel","harold","brittany")

age_res <- get_name_data("_age_1",names)
e_res <- get_name_data("_e_1",names)
p_res <- get_name_data("_p_1",names)
a_res <- get_name_data("_a_1",names)

e <- e_res[,mean(value),by=variable]
setnames(e,"V1","e")
p <- p_res[,mean(-value),by=variable]
setnames(p, "V1","p")
a <- a_res[,mean(value),by=variable]
setnames(a, "V1","a")
epa_names <- merge(merge(e,p),a)
setnames(epa_names, "variable","term")

# Note: there's already a brittany and johnny in the data!  and our estimates are comparable!
epa_data[term %in% epa_names]
epa_data <- epa_data[!(term %in% names)]
epa_data <- rbind(epa_data, epa_names)

# get deflection for i_a, using name modifier
i_a_affective <- experiment_details_trait[,.(i_a,actor,behavior,object)]
setnames(i_a_affective, "i_a", "choice")
i_a_affective[, deflection := get_deflection_for_traits(.SD),by=1:nrow(i_a_affective)]
experiment_details_trait[, i_a_deflection := i_a_affective$deflection]

# get deflection for i_b, using name modifier
i_b_affective <- experiment_details_trait[,.(i_b,actor,behavior,object)]
setnames(i_b_affective, "i_b", "choice")
i_b_affective[, deflection := get_deflection_for_traits(.SD),by=1:nrow(i_b_affective)]
experiment_details_trait[, i_b_deflection := i_b_affective$deflection]

#melt all the answers from all survey respondents into a single format
question_answers <- melt(survey_data[,experiment_details_trait$question_name,with=F] ,
                         measure=experiment_details_trait$question_name,
                         variable.factor = F)
question_answers[, value := sub("^a[n]? ","",value)]
question_answers[, value := sub("</span","",value)]
question_answers <- merge(experiment_details_trait[,.(question_name,i_a)],question_answers, by.x="question_name",by.y="variable")
question_answers[, sel := value == i_a]
results <- question_answers[,get_cint(.SD),by=question_name]

# gen comb results
experiment_details_trait[, trait_diff := ifelse(actor == "someone",0, 
                                                ifelse(actor == "ethel",3,
                                                       ifelse(actor=="harold",2,1)))]
results_comb_trait <- gen_comb_predictions(survey_data, experiment_details_trait,"trait_diff")
summary(results_comb_trait[['model']])
# gen trait alone results
experiment_details_trait[,is_young := grepl("Young",trait_cond)]
experiment_details_trait[,is_female := grepl("Female",trait_cond)]
experiment_details_trait[,has_trait := trait_cond != "No trait"]

results_trait <- gen_trait_model_results(survey_data,experiment_details_trait)
summary(results_trait[['model']])

# gen weighted act_results 
results_act_weighted <- gen_act_model_results(survey_data,experiment_details_trait)


# Merge in/make predictions
experiment_details_trait[, name := paste("If",capitalize(actor),paste0(behavior,"s an"), object, "\nis (s)he a",i_a, "or a",paste0(i_b,"?"))]
experiment_details_trait$name <- sub("Someone","someone",experiment_details_trait$name)
experiment_details_trait$name <- sub("talk tos","talks to",experiment_details_trait$name)
experiment_details_trait$name <- sub("an person","a person",experiment_details_trait$name)
experiment_details_trait <- merge(experiment_details_trait, results, by="question_name")
experiment_details_trait <- merge(experiment_details_trait, results_trait[['results']], by.x="question_name",by.y="variable")
experiment_details_trait <- merge(experiment_details_trait, results_comb_trait[['results']], by.x="question_name",by.y="variable")
experiment_details_trait <- merge(experiment_details_trait, results_act_weighted[['results']], by.x="question_name",by.y="variable")

experiment_details_trait[, prediction_affect := 1/(1+ exp(i_a_deflection-i_b_deflection))]

experiment_details_trait[, prediction_pcshc := ifelse(actor == "someone",.5,1)]


#####################################################################
#####################################################################
# Okay, analyze yo results
#####################################################################
#####################################################################
#####################################################################

#####################################################################
# Manipulation checks
#####################################################################

# age manipulation worked, although johnny and brittany are a bit older than expected
ggplot(age_res, aes(variable, value)) + geom_violin() + stat_summary(fun.data="mean_cl_boot")

# EPA values make sense
ggplot(e_res, aes(variable, value)) + geom_violin() + stat_summary(fun.data="mean_cl_boot")
ggplot(p_res, aes(variable, -value)) + geom_violin() + stat_summary(fun.data="mean_cl_boot")
ggplot(a_res, aes(variable, value)) + geom_violin() + stat_summary(fun.data="mean_cl_boot")

############## Comparing ACT to PCSMs  ###########

#### Claim 1

fit_act_pcsm_claim_1 <- rbind(experiment_details_assoc[assoc_cond %in% c("Medium (Institution)","High (Role Pair)") 
                                                       & affect_cond == "Low",
                                                       .(mean, prediction_affect,prediction_pcs_est,prediction_pcshc)],
                              experiment_details_trait[trait_cond != "No trait" & affect_cond == "Low",
                                                       .(mean, prediction_affect,prediction_pcs_est,prediction_pcshc)])
fit_act_pcsm_claim_1[, ACT := abs(prediction_affect-mean)]
fit_act_pcsm_claim_1[, PCS_EST := abs(prediction_pcs_est-mean)]
fit_act_pcsm_claim_1[, PCS_HC := abs(prediction_pcshc-mean)]
fit_act_pcsm_claim_1[, mean(PCS_EST)]
fit_act_pcsm_claim_1[, mean(PCS_HC)]
fit_act_pcsm_claim_1[, mean(ACT)]

with(fit_act_pcsm_claim_1,t.test(ACT,PCS_EST,"greater"))
with(fit_act_pcsm_claim_1,t.test(ACT,PCS_HC,"greater"))


#### Claim 2

fit_act_pcsm_claim_2 <- rbind(experiment_details_assoc[assoc_cond == "None" ,
                                                       .(mean, prediction_affect,prediction_pcs_est,prediction_pcshc)],
                              experiment_details_trait[trait_cond == "No trait",
                                                       .(mean, prediction_affect,prediction_pcs_est,prediction_pcshc)])
fit_act_pcsm_claim_2[, ACT := abs(prediction_affect-mean)]
fit_act_pcsm_claim_2[, PCS_EST := abs(prediction_pcs_est-mean)]
fit_act_pcsm_claim_2[, PCS_HC := abs(prediction_pcshc-mean)]
fit_act_pcsm_claim_2[, mean(PCS_EST)]
fit_act_pcsm_claim_2[, mean(PCS_HC)]
fit_act_pcsm_claim_2[, mean(ACT)]
with(fit_act_pcsm_claim_2,t.test(PCS_EST,ACT,"greater"))
with(fit_act_pcsm_claim_2,t.test(PCS_HC,ACT,"greater"))



########## Overall results ###############

overall <- rbind(experiment_details_assoc[, .(name,mean,assoc_cond, prediction_affect,prediction_pcs_est,prediction_comb, prediction_pcshc)],
                 experiment_details_trait[,.(name,mean, trait_cond,prediction_affect,prediction_pcs_est,prediction_comb, prediction_pcshc)],fill=T)
#overall <- overall[trait_cond != "No trait" | assoc_cond != "None"]
smean.cl.boot(abs(overall$prediction_comb-overall$mean))
smean.cl.boot(abs(overall$prediction_affect-overall$mean))
smean.cl.boot(abs(overall$prediction_pcs_est-overall$mean))
smean.cl.boot(abs(overall$prediction_pcshc-overall$mean))

######### Plot ####
theme_set(theme_bw(20))
experiment_details_assoc[, assoc_cond := factor(assoc_cond,
                                                labels=c("None",
                                                         "Low\n(Diff. Institution)",
                                                         "Medium\n(Institution)",
                                                         "High\n(Role Pair)"))]
experiment_details_assoc[, affect_cond := factor(affect_cond, 
                                                 levels=c("Low","High"), 
                                                 labels=c("Low\nAff. Defl.",  
                                                          "High\nAff. Defl."))]
experiment_details_trait[, affect_cond := factor(affect_cond, 
                                                 levels=c("Low","High"), 
                                                 labels=c("Low\nAff. Defl.",  
                                                          "High\nAff. Defl."))]

experiment_details_trait[, trait_cond := factor(trait_cond, 
                                                labels=c("None",
                                                          "Old\nFemale",
                                                         "Old\nMale",
                                                         "Young\nFemale",
                                                         "Young\nMale"))]


ggsave("img/assoc_full.pdf",full_res_plot(experiment_details_assoc,"assoc_cond"),h=17,w=11)

ggsave("img/trait_full.pdf",full_res_plot(experiment_details_trait,"trait_cond"),h=12,w=13)

p1a<- error_plot(experiment_details_assoc,"assoc_cond")
p2a <- error_plot(experiment_details_trait,"trait_cond")

ggsave("img/err.pdf", plot_grid(p1a,p2a,
                                labels = c('A','B'),
                                label_size = 40,
                                label_colour = 'red',
                                label_x = 0.05,
                                nrow = 1), h=10,w=16)
writeLines(capture.output(session_info() ), "sessionInfo.txt")

