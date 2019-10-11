# libs ----

library(data.table)
library(pander)
library(schoRsch)
library(pROC)
library(ggplot2)
library(sizeMat)
library(plyr)
library(MBESS)
library(ez)
library(BayesFactor)
library(TOSTER)

# COLLECT DATA ----


ro = function(value, round_to = 2) {
    return(format(round(value, round_to), nsmall = round_to))
}
cohen_d_within = function(var1, var2, for_table = T){
    ttest = t.test(var1, var2, paired=T)
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    sm = ci.sm(ncp = ttest$statistic, N = length(var1), conf.level = .95)
    d = sm$Standardized.Mean
    if (for_table == T) {
        out = paste("t(", df, ")", " = ", ro(t), ", p = ", ro(pvalue,3), ", dwithin = ", ro(d, 2), " [", ro(sm$Lower.Conf.Limit.Standardized.Mean, 2), ", ", ro(sm$Upper.Conf.Limit.Standardized.Mean, 2), "].", sep="")
    } else {
        out = paste("t(", df, ")", " = ", ro(t), ", p = ", ro(pvalue,3), ", dwithin = ", ro(d, 2), ", 95% CI [", ro(sm$Lower.Conf.Limit.Standardized.Mean, 2), ", ", ro(sm$Upper.Conf.Limit.Standardized.Mean, 2), "].", sep="")
    }
    print(out)
}

cohen_d_between = function(outcome, categ, for_table = T){
    ttest = t.test(outcome ~ categ)
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    table_categ = as.data.frame(table(categ))
    table_categ$Freq[table_categ$Freq == 0] = NA
    table_categ = na.omit(table_categ)
    n1 = table_categ[1,2]
    n2 = table_categ[2,2]
    the_smd = ci.smd(ncp = t, n.1 = n1, n.2 = n2, conf.level = .95)
    if (for_table == T) {
        out = paste("t(", ro(df,1), ")", " = ", ro(t), ", p = ", ro(pvalue, 3), ", dbetween = ", ro(the_smd$smd, 2), " [", ro(the_smd$Lower.Conf.Limit.smd, 2), ", ", ro(the_smd$Upper.Conf.Limit.smd, 2), "].", sep="")
    } else {
        out = paste("t(", ro(df,1), ")", " = ", ro(t), ", p = ", ro(pvalue, 3), ", dbetween = ", ro(the_smd$smd, 2), ", 95% CI [", ro(the_smd$Lower.Conf.Limit.smd, 2), ", ", ro(the_smd$Upper.Conf.Limit.smd, 2), "].", sep="")
    }
    print(out)
}
show = function(title, value, round_to = 2) {
    print(paste(title, "=", format(round(value, round_to), nsmall = round_to)))
}
show_auc = function(title, theroc, round_to = 3) {
    print(paste(title, " AUC = ", format(round(as.numeric(auc(theroc)), round_to), nsmall = round_to), " CI [", format(round(as.numeric(ci(theroc))[1], round_to), nsmall = round_to), "-", format(round(as.numeric(ci(theroc))[3], round_to), nsmall = round_to), "]", sep = ""))
}

#categs_to_test = list("countries","dates","animals")
#categs_to_test = list("countries","dates","animals")[1]

dir_path = 'C:/research/proj_lang/pilot'

setwd( paste(dir_path, 'lang_results', sep = '/') )
file_names = list.files(pattern = "^exp_ecit_lang.*txt$")

if ( exists("main_cit_merg") ) {
    rm(main_cit_merg)
    rm(dems_merg)
}

for(f_name in file_names){
    #f_name = "exp_ecit_lang_4_1_99_20190313205852.txt"

    print(f_name)

    subj_data = read.table(  f_name, sep="\t", header=TRUE, fill=TRUE, quote="\"")

    subj_data$stim_type <- as.character(subj_data$stim_type)
    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"
    subj_data$stim_type <- factor(subj_data$stim_type)

    subj_itms_base = subj_data[which( subj_data$phase == 'main' & subj_data$stim_type %in% list("probe", "irrelevant", "targetref", "nontargref", "target") ),]


    if (nrow(subj_itms_base) != 648) { # just double-check
        print("number of rows:")
        print(nrow(subj_itms_base))
        stop("trial num incorrect")
    }


    # if ( exists("exp_unique_names") ) {
    #     exp_unique_names =  merge( exp_unique_names, all_unique_names, all = T)
    # } else {
    #     exp_unique_names = all_unique_names
    # }

    # temp_raw = na.omit(subj_data, cols=c("incorrect"))
    # if ( exists("all_raw_data") ) {
    #     all_raw_data =  merge( all_raw_data, temp_raw, all = T)
    # } else {
    #     all_raw_data = temp_raw
    # }
    # all_raw_data = subset(all_raw_data, select=-c(stimulus_shown,date_in_ms))
    # write.table(all_raw_data, "C:/Users/Gaspar/Desktop/citapp_stuff/mobile_cit_full_row.txt", quote = F, row.names = F, sep="\t")

    subj_itms_base$valid_trial = ifelse(subj_itms_base$incorrect == 0 & subj_itms_base$too_slow == 0 & subj_itms_base$rt_start >= 150, 1, 0)
    subj_itms_base <- subj_itms_base[order(subj_itms_base$block_number, subj_itms_base$trial_number),]
    subj_cond = as.character(subj_itms_base$condition[1])
    subject_number = as.character(subj_itms_base$subject_id[1])


    subj_itms_base$famty = 0
    if (subj_cond %in% list(1,2) ) {
        subj_itms_base$famty[subj_itms_base$block_number %in% list(6,7) ] = 1
    } else {
        subj_itms_base$famty[subj_itms_base$block_number %in% list(4,5) ] = 1
    }
    # famty: "nonfamiliar" - 0; "familiar" - 1
    if (subj_cond %in% list(1,3) ) {
        subj_cond = 0
    } else {
        subj_cond = 1
    }
    # condition: "existing probe" - 0; "pseudo probe" - 1

    subj_itms_base$press_duration[subj_itms_base$press_duration == '-'] = NA
    subj_itms_base$press_duration = as.numeric( as.character(subj_itms_base$press_duration))

    subj_lin_parts = list()
    for (fam_state in c(0,1)) {

        subj_items = subj_itms_base[subj_itms_base$famty==fam_state,]

        subj_items = subj_items[subj_items$rt_start >= 150,]

        subj_valid_items = subj_items[subj_items$valid_trial==1,]

        subj_acc_rates <-aggregate( subj_items$valid_trial, by = list(subj_items$stim_type), FUN = mean)
        subj_acc_rates$Group.1 = paste("acc_rate", subj_acc_rates$Group.1, sep="_")
        subj_rt_mean <-aggregate(  subj_valid_items$rt_start, by = list(subj_valid_items$stim_type), FUN = mean)
        subj_rt_mean$Group.1 = paste("rt_mean", subj_rt_mean$Group.1, sep="_")
        subj_duration_rt_mean <-aggregate( subj_valid_items$press_duration, by = list(subj_valid_items$stim_type), FUN = mean, na.rm=TRUE)
        subj_duration_rt_mean$Group.1 = paste("duration_rt_mean", subj_duration_rt_mean$Group.1, sep="_")
        subj_lin_part = Reduce(function(x, y) merge(x, y, all=TRUE), list(subj_rt_mean, subj_duration_rt_mean, subj_acc_rates))
        subj_lin_part = dcast(setDT(subj_lin_part), .~Group.1, value.var=c("x"))

        #subj_lin_part$subj_coh_d_cit = cohen_d_cit(subj_valid_items$rt_start[subj_valid_items$stim_type == "probe"],  subj_valid_items$rt_start[subj_valid_items$stim_type == "irrelevant"])

        subj_lin_part$rt_mean_diffs = subj_lin_part$rt_mean_probe-subj_lin_part$rt_mean_irrelevant
        subj_lin_part$duration_rt_mean_diffs = subj_lin_part$duration_rt_mean_probe-subj_lin_part$duration_rt_mean_irrelevant
        subj_lin_part$acc_rate_diffs = subj_lin_part$acc_rate_probe-subj_lin_part$acc_rate_irrelevant

        colnames(subj_lin_part) <- paste(colnames(subj_lin_part), fam_state, sep = "_")
        names(subj_lin_part)[names(subj_lin_part) == paste0('._', fam_state)] <- "subject_id"

        subj_lin_parts[[fam_state+1]] = subj_lin_part
    }

    subj_overall_acc <-aggregate( subj_itms_base$valid_trial, by = list(subj_itms_base$stim_type), FUN = mean)
    subj_overall_acc$Group.1 = paste("overall_acc", subj_overall_acc$Group.1, sep="_")
    subj_overall_acc = dcast(setDT(subj_overall_acc), .~Group.1, value.var=c("x"))
    subj_overall_acc$main_overall_acc = ((subj_overall_acc$overall_acc_probe) + subj_overall_acc$overall_acc_irrelevant * 4) / 5
    names(subj_overall_acc)[names(subj_overall_acc) == "."] <- "subject_id"

    subject_line = merge(subj_lin_parts[[1]], subj_lin_parts[[2]], by = "subject_id", all = TRUE)
    subject_line = merge(subject_line, subj_overall_acc, by = "subject_id", all = TRUE)

    subject_line$subject_id = subject_number
    subject_line$condition = subj_cond

    dems_string = subj_data[ startsWith(as.character(subj_data$subject_id), 'dems'), ]$condition
    dems = c( subject_number, strsplit( as.character(dems_string), "/")[[1]] )
    dems = do.call(rbind.data.frame, list(dems) )
    colnames(dems) = c("subject_id","gender","age","country","hand","rep1","rep2","rep3","drtn","dcit","correct_guess","guessed")

    if ( exists("main_cit_merg") ) { # add subject aggregations
        main_cit_merg =  merge( main_cit_merg, subject_line, all = T)
        dems_merg = merge( dems_merg, dems, all = T)
    } else {
        main_cit_merg = subject_line
        dems_merg = dems
    }
}

dems_merg$subject_id = as.character(dems_merg$subject_id)

main_cit_merg = merge(x = main_cit_merg, y = dems_merg, by = "subject_id", all = TRUE)

main_cit_data <- main_cit_merg[order(main_cit_merg$condition, main_cit_merg$subject_id),]

main_cit_data = data.table(main_cit_data)

# main_cit_data = main_cit_data[!(main_cit_data$subject_id %in% failed_probe_check),]

main_cit_data = main_cit_data[is.na(main_cit_data$overall_acc_target) | main_cit_data$overall_acc_target > 0.5,]
main_cit_data = main_cit_data[is.na(main_cit_data$overall_acc_targetref) | main_cit_data$overall_acc_targetref > 0.5,]
main_cit_data = main_cit_data[is.na(main_cit_data$overall_acc_nontargref) | main_cit_data$overall_acc_nontargref > 0.5,]
main_cit_data = main_cit_data[main_cit_data$main_overall_acc > 0.75,]

main_cit_merg$remaining = ifelse(main_cit_merg$subject_id %in% main_cit_data$subject_id, 1, 0)
excluded_nums = do.call(data.frame, aggregate( main_cit_merg$overall_acc_targetref, by = list(main_cit_merg$condition, main_cit_merg$remaining), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
for(i in 1:nrow(excluded_nums)) {
    row <- excluded_nums[i,]
    print(paste('for Condition:', row[1], 'remaining:', row[2], 'count', round(row[3],2), 'LexTALE score =', round(row[4],2), '+', round(row[5],2)))
}


full_data = main_cit_data[main_cit_data$correct_guess == 4,]

main_cit_data$remaining = ifelse(main_cit_data$subject_id %in% full_data$subject_id, 1, 0)
#norecall_nums = do.call(data.frame, aggregate( main_cit_data$subject_id, by = list(main_cit_data$condition, main_cit_data$remaining), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
# for(i in 1:nrow(norecall_nums)) {
#     row <- norecall_nums[i,]
#     print(paste('for Condition:', row[1], 'remaining:', row[2], 'count', round(row[3],2), 'd-CIT =', round(row[4],2), '+', round(row[5],2)))
# }


# ANALYSIS ----


# within famty: "nonfamiliar" - 0; "familiar" - 1
# between condition: "existing probe" - 0; "pseudo probe" - 1

print("effects per between condition:")
for(cond in list(0,1)) {  # 0,1,2,3,4,5
    cit_data_cond = full_data[condition==cond,]

    print("::::::::::::::::::::::::::::::CONDITION - existing (0) or pseudo (1) ::::::::::::::::::::::::::::::::::::")
    print(cond)
    for (within_cond in list("0","1") ) { # 0,1,2,3,4,5
        rt_probe = cit_data_cond[[paste("rt_mean_probe", within_cond, sep="_")]]
        rt_irr = cit_data_cond[[paste("rt_mean_irrelevant", within_cond, sep="_")]]

        acc_probe = cit_data_cond[[paste("acc_rate_probe", within_cond, sep="_")]]
        acc_irr = cit_data_cond[[paste("acc_rate_irrelevant", within_cond, sep="_")]]

        hold_probe = cit_data_cond[[paste("duration_rt_mean_probe", within_cond, sep="_")]]
        hold_irr = cit_data_cond[[paste("duration_rt_mean_irrelevant", within_cond, sep="_")]]

        print("::::::::::::::::::::::::::::::CONDITION - nonfamiliar (0) or familiar (1)::::::::::::::::::::::::::::::::::::")
        print(within_cond)

        print("mean RT:")
        cohen_d_within(rt_probe,rt_irr)
        print(".")
        print("Acc:")
        cohen_d_within(acc_probe,acc_irr)
        print(".")
        print("Hold:")
        cohen_d_within(hold_probe,hold_irr)
        print("")
    }
}

print("effects per within condition:")


print("Within differences:")
for (categ in list(list("0","1")) ) {
    rt_cat1 = full_data[[paste("rt_mean_diffs", categ[1], sep="_")]]
    rt_cat2 = full_data[[paste("rt_mean_diffs", categ[2], sep="_")]]
    acc_cat1 = full_data[[paste("acc_rate_diffs", categ[1], sep="_")]]
    acc_cat2 = full_data[[paste("acc_rate_diffs", categ[2], sep="_")]]
    hold_cat1 = full_data[[paste("duration_rt_mean_diffs", categ[1], sep="_")]]
    hold_cat2 = full_data[[paste("duration_rt_mean_diffs", categ[2], sep="_")]]

    print(paste("existing:", mean(rt_cat1), sd(rt_cat1)))
    print(paste("pseudo:", mean(rt_cat2), sd(rt_cat2)))
    print(paste("------------------------Categories:", categ[1], categ[2]))

    #print(dataTOSTpaired(data = full_data, pairs = list(c(i1="rt_mean_diffs_0",i2="rt_mean_diffs_1")), low_eqbound = -0.4, high_eqbound = 0.4, alpha = 0.05, plots = TRUE))
    print(as.vector( ttestBF(rt_cat1,rt_cat2, paired = T) ))
    print(as.vector( ttestBF(acc_cat1,acc_cat2, paired = T) ))

    print("mean RT:")
    cohen_d_within(rt_cat1,rt_cat2, F)
    print(".")
    print("Acc:")
    cohen_d_within(acc_cat1,acc_cat2, F)
    print(".")
    print("Hold:")
    cohen_d_within(hold_cat1,hold_cat2, F)
    print("")

}

## BETWEEN


# within famty: "nonfamiliar" - 0; "familiar" - 1
# between condition: "existing probe" - 0; "pseudo probe" - 1

data_long <- melt(full_data, measure.vars = c("rt_mean_diffs_0","rt_mean_diffs_1"), variable.name = "var_types", value.name = "rt_mean_values" )
data_long$subject_id = as.factor( tolower( as.character( data_long$subject_id ) ) )
data_long$condition = as.factor(data_long$condition)
data_long$famty = as.factor( ifelse(grepl( '_0', data_long$var_types ), "nonfamiliar", "familiar") )

rt_anova = ezANOVA(data_long, rt_mean_values, subject_id, within = c(famty), between = c(condition), type = 2, detailed = TRUE, return_aov = TRUE)
anova_out(rt_anova)

#anovaBF(rt_mean_values ~ block*condition + subject_id, data = data_long, whichRandom = "subject_id", whichModels = 'bottom')

anova_plot = ezPlot(
    data = data_long
    , dv = .(rt_mean_values)
    , wid = .(subject_id)
    , within = c(famty)
    , between = c(condition)
    , x = .(condition)
    , split = .(famty)
    , x_lab = 'condition'
    , y_lab = 'RT (ms)'
    , split_lab = 'famty'
)
print(anova_plot)

# SAVE -----

write.table(full_data, "C:/Users/Gaspar/Desktop/citapp_stuff/data_for_spss.txt", quote = F, row.names = F, sep="\t")

# DEMS -----
age = do.call(data.frame, aggregate( full_data$age, by = list(full_data$condition), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
for(i in 1:nrow(age)) {
    row <- age[i,]
    print(paste('for Condition:', row[1], 'count', round(row[2],2), 'age =', round(row[3],2), '+', round(row[4],2)))
}

for_gender = full_data[,c("condition","gender")]
gender = data.frame(prop.table(table(for_gender), 1))
for(i in 1:(nrow(gender)/2)) {
    row <- gender[i,]
    print(paste('for Condition:', row[1], 'male', format(round(row[3]*100,2), nsmall = 2), "%"))
}


# final summary ----

full_data$condition = full_data$condition.y

all_probe_rt_means_0 <- do.call(data.frame, aggregate( full_data$rt_mean_probe_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_probe_rt_means_0$rt_mean_probe_0 = paste(all_probe_rt_means_0$x.mean, all_probe_rt_means_0$x.sd, sep="±")
all_probe_rt_means_0 = subset(all_probe_rt_means_0, select=-c(x.mean,x.sd))

all_irr_rt_means_0 <- do.call(data.frame,  aggregate( full_data$rt_mean_irrelevant_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_irr_rt_means_0$rt_mean_irr_0 = paste(all_irr_rt_means_0$x.mean, all_irr_rt_means_0$x.sd, sep="±")
all_irr_rt_means_0 = subset(all_irr_rt_means_0, select=-c(x.mean,x.sd))

all_target_rt_means_0 <- do.call(data.frame, aggregate( full_data$rt_mean_target_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_target_rt_means_0$rt_mean_target_0 = paste(all_target_rt_means_0$x.mean, all_target_rt_means_0$x.sd, sep="±")
all_target_rt_means_0 = subset(all_target_rt_means_0, select=-c(x.mean,x.sd))

all_selfref_rt_means_0 <- do.call(data.frame, aggregate( full_data$rt_mean_targetref_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_selfref_rt_means_0$rt_mean_selfref_0 = paste(all_selfref_rt_means_0$x.mean, all_selfref_rt_means_0$x.sd, sep="±")
all_selfref_rt_means_0 = subset(all_selfref_rt_means_0, select=-c(x.mean,x.sd))

all_otherref_rt_means_0 <- do.call(data.frame, aggregate( full_data$rt_mean_nontargref_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_otherref_rt_means_0$rt_mean_otherref_0 = paste(all_otherref_rt_means_0$x.mean, all_otherref_rt_means_0$x.sd, sep="±")
all_otherref_rt_means_0 = subset(all_otherref_rt_means_0, select=-c(x.mean,x.sd))


#

all_probe_acc_rates_0 <- do.call(data.frame, aggregate( full_data$acc_rate_probe_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_probe_acc_rates_0$acc_rate_probe_0 = paste(all_probe_acc_rates_0$x.mean, all_probe_acc_rates_0$x.sd, sep="±")
all_probe_acc_rates_0 = subset(all_probe_acc_rates_0, select=-c(x.mean,x.sd))

all_irr_acc_rates_0 <- do.call(data.frame, aggregate( full_data$acc_rate_irrelevant_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_irr_acc_rates_0$acc_rate_irr_0 = paste(all_irr_acc_rates_0$x.mean, all_irr_acc_rates_0$x.sd, sep="±")
all_irr_acc_rates_0 = subset(all_irr_acc_rates_0, select=-c(x.mean,x.sd))


all_target_acc_rates_0 <- do.call(data.frame, aggregate( full_data$acc_rate_target_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_target_acc_rates_0$acc_rate_target_0 = paste(all_target_acc_rates_0$x.mean, all_target_acc_rates_0$x.sd, sep="±")
all_target_acc_rates_0 = subset(all_target_acc_rates_0, select=-c(x.mean,x.sd))


all_selfref_acc_rates_0 <- do.call(data.frame, aggregate( full_data$acc_rate_targetref_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_selfref_acc_rates_0$acc_rate_selfref_0 = paste(all_selfref_acc_rates_0$x.mean, all_selfref_acc_rates_0$x.sd, sep="±")
all_selfref_acc_rates_0 = subset(all_selfref_acc_rates_0, select=-c(x.mean,x.sd))

all_otherref_acc_rates_0 <- do.call(data.frame, aggregate( full_data$acc_rate_nontargref_0, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_otherref_acc_rates_0$acc_rate_otherref_0 = paste(all_otherref_acc_rates_0$x.mean, all_otherref_acc_rates_0$x.sd, sep="±")
all_otherref_acc_rates_0 = subset(all_otherref_acc_rates_0, select=-c(x.mean,x.sd))

# 1

all_probe_rt_means_1 <- do.call(data.frame, aggregate( full_data$rt_mean_probe_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_probe_rt_means_1$rt_mean_probe_1 = paste(all_probe_rt_means_1$x.mean, all_probe_rt_means_1$x.sd, sep="±")
all_probe_rt_means_1 = subset(all_probe_rt_means_1, select=-c(x.mean,x.sd))

all_irr_rt_means_1 <- do.call(data.frame,  aggregate( full_data$rt_mean_irrelevant_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_irr_rt_means_1$rt_mean_irr_1 = paste(all_irr_rt_means_1$x.mean, all_irr_rt_means_1$x.sd, sep="±")
all_irr_rt_means_1 = subset(all_irr_rt_means_1, select=-c(x.mean,x.sd))

all_target_rt_means_1 <- do.call(data.frame, aggregate( full_data$rt_mean_target_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_target_rt_means_1$rt_mean_target_1 = paste(all_target_rt_means_1$x.mean, all_target_rt_means_1$x.sd, sep="±")
all_target_rt_means_1 = subset(all_target_rt_means_1, select=-c(x.mean,x.sd))

all_selfref_rt_means_1 <- do.call(data.frame, aggregate( full_data$rt_mean_targetref_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_selfref_rt_means_1$rt_mean_selfref_1 = paste(all_selfref_rt_means_1$x.mean, all_selfref_rt_means_1$x.sd, sep="±")
all_selfref_rt_means_1 = subset(all_selfref_rt_means_1, select=-c(x.mean,x.sd))

all_otherref_rt_means_1 <- do.call(data.frame, aggregate( full_data$rt_mean_nontargref_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x),0), sd = ro(sd(x), 0 ) )) )
all_otherref_rt_means_1$rt_mean_otherref_1 = paste(all_otherref_rt_means_1$x.mean, all_otherref_rt_means_1$x.sd, sep="±")
all_otherref_rt_means_1 = subset(all_otherref_rt_means_1, select=-c(x.mean,x.sd))


#

all_probe_acc_rates_1 <- do.call(data.frame, aggregate( full_data$acc_rate_probe_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_probe_acc_rates_1$acc_rate_probe_1 = paste(all_probe_acc_rates_1$x.mean, all_probe_acc_rates_1$x.sd, sep="±")
all_probe_acc_rates_1 = subset(all_probe_acc_rates_1, select=-c(x.mean,x.sd))

all_irr_acc_rates_1 <- do.call(data.frame, aggregate( full_data$acc_rate_irrelevant_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_irr_acc_rates_1$acc_rate_irr_1 = paste(all_irr_acc_rates_1$x.mean, all_irr_acc_rates_1$x.sd, sep="±")
all_irr_acc_rates_1 = subset(all_irr_acc_rates_1, select=-c(x.mean,x.sd))


all_target_acc_rates_1 <- do.call(data.frame, aggregate( full_data$acc_rate_target_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_target_acc_rates_1$acc_rate_target_1 = paste(all_target_acc_rates_1$x.mean, all_target_acc_rates_1$x.sd, sep="±")
all_target_acc_rates_1 = subset(all_target_acc_rates_1, select=-c(x.mean,x.sd))


all_selfref_acc_rates_1 <- do.call(data.frame, aggregate( full_data$acc_rate_targetref_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_selfref_acc_rates_1$acc_rate_selfref_1 = paste(all_selfref_acc_rates_1$x.mean, all_selfref_acc_rates_1$x.sd, sep="±")
all_selfref_acc_rates_1 = subset(all_selfref_acc_rates_1, select=-c(x.mean,x.sd))

all_otherref_acc_rates_1 <- do.call(data.frame, aggregate( full_data$acc_rate_nontargref_1, by = list(full_data$condition), function(x) c(mean = ro(mean(x*100),1), sd = ro(sd(x*100),1))) )
all_otherref_acc_rates_1$acc_rate_otherref_1 = paste(all_otherref_acc_rates_1$x.mean, all_otherref_acc_rates_1$x.sd, sep="±")
all_otherref_acc_rates_1 = subset(all_otherref_acc_rates_1, select=-c(x.mean,x.sd))

#-

main_results = Reduce(function(x, y) merge(x, y, by = "Group.1", all=TRUE), list(all_probe_rt_means_0, all_irr_rt_means_0, all_target_rt_means_0, all_selfref_rt_means_0, all_otherref_rt_means_0, all_probe_acc_rates_0, all_irr_acc_rates_0, all_target_acc_rates_0, all_selfref_acc_rates_0, all_otherref_acc_rates_0, all_probe_rt_means_1, all_irr_rt_means_1, all_target_rt_means_1, all_selfref_rt_means_1, all_otherref_rt_means_1, all_probe_acc_rates_1, all_irr_acc_rates_1, all_target_acc_rates_1, all_selfref_acc_rates_1, all_otherref_acc_rates_1))
names(main_results)[names(main_results) == "Group.1"] <- "condition"


write.table(main_results, "C:/ecit/main_stats_table.txt", quote = F, row.names = F, sep="\t")
