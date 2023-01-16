library('data.table')
str_fun = function (x, oper) {
    if (is.na(x) | x == 0) {
        return(0)
    } else {
        return(oper(as.numeric(
            strsplit(as.character(x), '[;]|[+]')[[1]]
        )))
    }
}
str_sum = function(x) {
    return(str_fun(x, sum))
}
str_mean = function(x) {
    return(str_fun(x, mean))
}
str_count = function(x) {
    return(str_fun(x, length))
}


title_match = function(short, full) {
    full = substr(full, 0, nchar(short))
    if (nchar(full) > 10) {
        short = substr(full, 0, nchar(full))
    }
    tolower(full_data$title)
    return((adist(tolower(full), tolower(short)) < 4)[1])
}


setwd(neatStats::path_neat('phase3'))
f_name = "online_offline_studies_phase3_ALL.xlsx" # get all result file names

gs_dat = data.table()

for (xls_tab in readxl::excel_sheets(f_name)) {
    sheet_dat = as.data.table(readxl::read_xlsx(f_name, sheet = xls_tab))
    sheet_dat$journal = xls_tab
    sheet_dat = sheet_dat[!is.na(sheet_dat$year)]
    gs_dat = rbindlist(list(gs_dat, sheet_dat), fill = TRUE)
}


gs_dat = Filter(function(x)!all(is.na(x)), gs_dat)

gs_dat = gs_dat[!grepl('SKIP', gs_dat$notes, fixed = TRUE), ]

gs_dat$offline_sum = unname(sapply(unlist(gs_dat$offline), str_sum))
gs_dat$online_sum = unname(sapply(unlist(gs_dat$online), str_sum))
gs_dat$offline_mean = unname(sapply(unlist(gs_dat$offline), str_mean))
gs_dat$online_mean = unname(sapply(unlist(gs_dat$online), str_mean))
gs_dat$offline_count = unname(sapply(unlist(gs_dat$offline), str_count))
gs_dat$online_count = unname(sapply(unlist(gs_dat$online), str_count))

gs_dat$doi = tolower(
    sub(
        'https://doi.org/|http://dx.doi.org/|https://doi/|https://psycnet.apa.org/doi/',
        '',
        trimws(iconv(
            gs_dat$doi, from = 'UTF-8', to = 'ASCII//TRANSLIT'
        ))
    )
)

# str(gs_dat)

if (anyDuplicated(gs_dat$doi)) {
    stop('doi duplicated ',
         paste(gs_dat$doi[duplicated(gs_dat$doi)], collapse = ', '))
}

# cref_dat = as.data.table(rcrossref::cr_works(dois = unique(gs_dat$doi))$data)
cref_dat = as.data.table(rcrossref::cr_works(dois = gs_dat$doi, .progress = 'time')$data)
cref_dat_list = list()
for (cref_doi in cref_dat$doi) {
    cref_datx = cref_dat[doi == cref_doi]
    cref_dat_list[[length(cref_dat_list) + 1]] =
        c(
            doi = cref_datx$doi,
            journal2 = cref_datx$container.title,
            date = cref_datx$published.print,
            issued = cref_datx$issued,
            citations = as.numeric(cref_datx$is.referenced.by.count),
            title_full = cref_datx$title,
            authors = nrow(cref_datx$author[[1]])
        )
}

cref_table = as.data.table(do.call(rbind, cref_dat_list))
cref_table$doi = tolower(sub(
    'https://doi.org/|http://dx.doi.org/|https://doi/',
    '',
    trimws(iconv(
        cref_table$doi, from = 'UTF-8', to = 'ASCII//TRANSLIT'
    ))
))
full_data = merge(gs_dat,
                  cref_table,
                  by = 'doi',
                  all.x = TRUE)

# check missing CREF entries: noref = full_data[is.na(full_data$citations)]

for (replacer in list(
    c('&amp;', 'and'),
    c('Journal of Experimental Psychology:', 'JEP'),
    c(' PRPF', ''),
    c(' Psychologische Forschung', '')
)) {
    full_data$journal2 = sub(replacer[1], replacer[2], full_data$journal2)
}
to_check = full_data[!is.na(full_data$journal2),]
if (any(to_check$journal != to_check$journal2)) {
    stop('journal mismatch ',
         paste(to_check$doi[to_check$journal != to_check$journal2], collapse = ', '))
}
if (!all(mapply(title_match, to_check$title, to_check$title_full))) {
    stop('title mismatch ',
         paste(to_check$title[!mapply(title_match,
                                      to_check$title, to_check$title_full)], collapse = ', '))
}

# just to check cref and recorded year correspondence
# (the two excluded DOIs were manually checked: the crossref data seems incorrect)
cref_years = full_data[(!is.na(full_data$date.y)) & !(full_data$doi %in% c('10.1007/s00426-006-0074-2', '10.1007/s00426-006-0077-z'))]
if (!all(cref_years$year == as.numeric(substr(cref_years$date.y, 1, 4)))) {
    stop('year mismatch ',
         paste(cref_years$doi[!cref_years$year == as.numeric(substr(cref_years$date.y, 1, 4))], collapse = ', '))
}
cref_years_JEP = full_data[full_data$journal == 'JEP General']
if (!all(cref_years_JEP$year == as.numeric(substr(cref_years_JEP$issued, 1, 4)))) {
    stop('year mismatch ',
         paste(cref_years_JEP$doi[!cref_years_JEP$year == as.numeric(substr(cref_years_JEP$issued, 1, 4))], collapse = ', '))
}


full_data$journal2 = NULL
full_data$title = full_data$title_full
full_data$title_full = NULL

saveRDS(full_data,
        neatStats::path_neat('online_vs_offline_data.rds'))
