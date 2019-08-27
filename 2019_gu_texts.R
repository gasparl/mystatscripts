library("neatStats")

setwd(script_path())

text_data = get(load("final_data_concreteness_06022018.RData"))[,1:10]

#unique(text_data$topic)
#names(text_data)
text_data = text_data[text_data$topic %in% c("opinion_neg", "opinion_pos", "past_weekend"),][, c("Filename", "text", "veracity", "topic")]

text_data$w_count = sapply(gregexpr("\\S+", text_data$text), length)

text_data$new_filename = gsub(pattern = ".txt", x = text_data$Filename, replacement = "_gu.txt")

text_data = text_data[text_data$w_count >= 80 & text_data$w_count <= 140,]

unique(text_data$topic)

text_data_final = text_data[text_data$topic == "past_weekend",][1:6,]
apply(text_data_final, 1, function(x) write.table(data.frame(x["text"]), file = x["new_filename"], row.names = FALSE, quote = FALSE, col.names = FALSE))

cat( text_data$new_filename, sep = "', '")

text_clean <- data.frame(lapply(text_data, function(x) { 
    gsub("[\r\n\t]", "", x) }))

    
write.table(
    text_clean[text_clean$topic == "past_weekend",],
    "clipboard",
    sep = "\t",
    quote = F,
    row.names = F
)

neatStats::aggr_neat(
    text_data,
    values = w_count,
    method = median,
    group_by = c('topic', "veracity")
)
