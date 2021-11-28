setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('tidyverse')

files <- list.files(path = 'raw_data/pre_processed_data', pattern = '*.csv', full.names = TRUE)

data <- list()

week_name <- function (X) {
  sapply(X = X, FUN = function (x) {
      if (nchar(x) == 2) as.numeric(substr(x, 2, 2)) else as.numeric(substr(x, 2, 3))
    })
}

for (i in 1:length(files)) {

  temp_data <- read_csv(file = files[i])
  
  temp_data <- temp_data %>% 
                 rename(Region = region, Year = year) %>% rename_with(.fn = week_name, .cols = starts_with(match = 'w')) %>% 
                 pivot_longer(cols = 3:(ncol(temp_data)), names_to = 'Week', values_to = 'Count') %>% 
                 mutate(Count = replace_na(Count, 0)) %>%
                 mutate(Week = as.numeric(Week))
  
  data[[i]] <- temp_data
  
}

week_seq <- function (Week, Year, n_regions) {
  
  years <- unique(Year)
  n_weeks <- sapply(X = years, FUN = function (y) { if ((y == 2014) | (y == 2020)) 53 else 52 }) # Years with 53 weeks
  
  week_seq_vec <- c()
  
  for (i in 1:(length(Week))) {
    y <- which(years == Year[i]) - 1
    
    if (y == 0) {
      week_seq_vec <- c(week_seq_vec, Week[i])
    } else {
      week_seq_vec <- c(week_seq_vec, Week[i] + sum(n_weeks[1:y]))
    }
    
  }
  week_seq_vec
}

data <- bind_rows(data) %>% 
          add_column(Index = 1:(nrow(.)), .before = 1) %>% 
          mutate(Week_seq = week_seq(Week = Week, Year = Year, n_regions = length(unique(Region))), .after = Week)

write_csv(x = data, file = 'brazil.csv')
