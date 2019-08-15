library("neatStats")

script_path()

text_data = get(load("final_data_concreteness_06022018.RData"))[,1:10]

unique(text_data$topic)
names(text_data)
text_data = text_data[text_data$topic %in% c("opinion_neg", "opinion_pos", "past_weekend"),][, c("Filename", "text", "veracity", "topic")]

text_data$w_count = sapply(gregexpr("\\S+", text_data$text), length)

neatStats::aggr_neat(
    text_data,
    values = w_count,
    method = median,
    group_by = c('topic', "veracity")
)
