# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat(''))

file_names = c()
pairs = list(
    c("ecit_2017_data/exp_all/", "^uvacit_c2.*txt$"),
    c("ecit_repl_data/", "^ecit_repl.*txt$"),
    c("cit_leak_exp2/", "^ECIT_Leak_1_.*txt$"),
    c("exp_ecit_keys/", "^exp_ecit_keys_.*txt$"),
    c("lang_results/exp12", "^exp_ecit_lang_.*txt$"),
    c("lang_results/exp4", "^lg_exp4.*txt$")
)

for (pair in pairs) {
    file_names = c(
        file_names,
        list.files(
            path = pair[1],
            pattern = pair[2],
            recursive = TRUE,
            full.names = TRUE
        )
    )
}

alltypes = c(
    "probe",
    "irrelevant1",
    "irrelevant2",
    "irrelevant3",
    "irrelevant4",
    "targetflr",
    "nontargflr",
    "target"
)
full_merged = data.frame()
for(f_name in enum(file_names)) {
    #f_name = c(0, "ecit_2017_data/exp_all//uvacit_c2_n479_sub_fw0778.txt")
    #f_name = c(0, "ecit_repl_data//ecit_repl_3_SEF_33870465.txt")
    #f_name = c(0, "cit_leak_exp2//ECIT_Leak_1_VEZ_44511947.txt")
    #f_name = c(0, "exp_ecit_keys//exp_ecit_keys_4_startnarrow_1_ord1_08_20190322090038.txt")
    #f_name = c(0, "lang_results/exp12/exp_ecit_lang_2_083_20191216092948.txt")
    #f_name = c(0, "lang_results/exp4/lg_exp4_hu_HIZ_20200620104858.txt")
    #
    cat(f_name, "; ")
    if (startsWith(f_name[2], "ecit_2017_data")) {
        subj_data = read.table(
            f_name[2],
            sep = "\t",
            header = FALSE, #
            fill = TRUE,
            quote = "\"",
            stringsAsFactors = FALSE
        )
    } else {
        subj_data = read.table(
            f_name[2],
            sep = "\t",
            header = TRUE,
            fill = TRUE,
            quote = "\"",
            stringsAsFactors = FALSE
        )
    }
    if (startsWith(f_name[2], "ecit_2017_data")) {
        colnames(subj_data) <- c("id", "date", "ip", "os", "browser", "age", "gender", "lang", "condition", "subject_id", "country", "edu", "rt_start", "stim", "stim_type", "category", "resp", "isi", "trial_number", "corr", "t1", "t2")
        subj_data$stim_type[subj_data$stim_type == "itarget"] = "targetflr"
        subj_data$stim_type[subj_data$stim_type == "nontarget"] = "nontargflr"
        subj_data$block_number = ifelse(subj_data$trial_number <= 162,
                                        1,
                                        ifelse(
                                            subj_data$trial_number <= 162 * 2,
                                            2,
                                            ifelse(subj_data$trial_number <= 162 *
                                                       3, 3, 4)
                                        ))
        subj_data$stimcat = paste0(subj_data$stim, subj_data$category)
        stim_dict = c()
        for (bnum in unique(subj_data$block_number)) {
            bdata = subj_data[subj_data$block_number == bnum &
                                  subj_data$stim_type == 'irrelevant',]
            # bdata = subj_data[subj_data$block_number == 1 &
            #                       subj_data$stim_type == 'irrelevant', ]
            scount = 0
            for (stm in unique(bdata$stimcat)) {
                scount = scount + 1
                stim_dict[stm] = scount
            }
        }
        subj_data$stim_type[subj_data$stim_type == 'irrelevant'] = paste0(subj_data$stim_type,
                                                                          stim_dict[subj_data$stimcat])[subj_data$stim_type == 'irrelevant']
        subj_data$too_slow = ifelse(subj_data$rt_start > 800, 1, 0)
        subj_data$incorrect = ifelse(subj_data$corr == 0, 1, 0)
        subj_data$exp = "LKV 2017"
    } else if (startsWith(f_name[2], "ecit_repl_data")) {
        subj_data$stim_type[subj_data$stim_type == "selfrefitem"] = "targetflr"
        subj_data$stim_type[subj_data$stim_type == "otherrefitem"] = "nontargflr"
        subj_data = subj_data[subj_data$block_number %in% c(4, 5, 6, 10, 11, 12) &
                                  subj_data$cit_type == 1, ]
        subj_data$rt_start = subj_data$rt
        subj_data$isi = subj_data$isi_delay
        subj_data$exp = "L 2021"
    } else if (startsWith(f_name[2], "cit_leak_exp2")) {
        subj_data$stim_type[subj_data$stim_type == "selfrefitem"] = "targetflr"
        subj_data$stim_type[subj_data$stim_type == "otherrefitem"] = "nontargflr"
        subj_data = subj_data[subj_data$block_number %in% c(4, 5, 6, 7), ]
        subj_data$rt_start = subj_data$rt
        msdats = subj_data$date_in_ms
        subj_data$isi = NA
        subj_data$exp = "LA 2019"
    } else if (startsWith(f_name[2], "exp_ecit_keys")) {
        subj_data$stim_type[subj_data$stim_type == "targetref"] = "targetflr"
        subj_data$stim_type[subj_data$stim_type == "nontargref"] = "nontargflr"
        subj_data = subj_data[subj_data$block_number %in% c(4, 5, 7, 8), ]
        subj_data$exp = "L (2019)"
    } else if (startsWith(f_name[2], "lang_results/exp12")) {
        subj_data$stim_type[subj_data$stim_type == "targetref"] = "targetflr"
        subj_data$stim_type[subj_data$stim_type == "nontargref"] = "nontargflr"
        subj_data = subj_data[subj_data$phase == 'main', ]
        subj_data$exp = "LKAF12 2021"
    } else if (startsWith(f_name[2], "lang_results/exp4")) {
        dems_row = subj_data[startsWith(as.character(subj_data$subject_id), 'dems'), ]
        dems_dat = strsplit(dems_row[[3]], "/")[[1]]
        if (dems_dat[3] == 'yes') {
            cat('0')
            next
        }
        subj_data = subj_data[subj_data$phase == 'main' &
                                  subj_data$tested_lang == dems_dat[2],]
        subj_data$exp = "LKAF4 2021"
    }
    subj_items = subj_data[subj_data$stim_type %in% alltypes,]
    for (bnum in unique(subj_items$block_number)) {
        blockdata  = subj_items[subj_items$block_number == bnum,]
        if (length(setdiff(alltypes, unique(blockdata$stim_type))) > 0) {
            stop("wrong items: ", unique(blockdata$stim_type))
        }
    }
    if (nrow(subj_items) < 200 || nrow(subj_items) > 800) {
        stop("number of rows:", nrow(subj_items))
    }
    subj_items$subject_id = paste(f_name[1], subj_items$subject_id, sep = '_')
    finals = subj_items[c(
        "exp",
        "subject_id",
        "block_number",
        "trial_number",
        "stim_type",
        "rt_start",
        "incorrect",
        "too_slow",
        "isi"
    )]
    full_merged = plyr::rbind.fill(full_merged, finals)
}

full_merged$stim_type =  sub('irrelevant', 'control', full_merged$stim_type)
saveRDS(full_merged, "scit_filler_meta.rds")

