library(rvest)


url <- "https://www.baseball-reference.com/leagues/MLB/1962-standard-pitching.shtml"

{
    start <- 1961
    end <- 2012
    n <- end - start
    
    Pitching <- as.list(NULL)
    for(i in 1:n+1){
        a <- "https://www.baseball-reference.com/leagues/MLB/"
        b <- i + (start-1)
        c <- "-standard-pitching.shtml"
        url <- paste(a,b,c,sep = "")
        webpage <- read_html(url)
        tbls <- html_nodes(webpage, "table")
        head(tbls)
        
        tbls_ls <- webpage %>%
            html_nodes("table") %>%
            .[1] %>%
            html_table(fill=TRUE)
        df <- as.data.frame(tbls_ls, header = TRUE)
        df$Year <- b
        Pitching[[i]] <- df
    }
}

pitch_data <- as.data.frame(NULL)
for(i in 2:52) {
    data <- Pitching[[i]]
    rows <- nrow(data)-2
    data <- data[1:rows,]
    pitch_data <- rbind(pitch_data,data)
}

pitch_data_no_lg <- as.data.frame(NULL)
for(i in 2:52) {
    data <- Pitching[[i]]
    rows <- nrow(data)-3
    data <- data[1:rows,]
    pitch_data_no_lg <- rbind(pitch_data_no_lg,data)
}

write.csv(pitch_data, "Pitching_Data.csv")
write.csv(pitch_data_no_lg, "Pitching_Data_No_LG.csv")
