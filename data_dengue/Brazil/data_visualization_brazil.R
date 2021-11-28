setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('tidyverse')

data <- read_csv(file = 'brazil.csv')

n_weeks <- sapply(X = unique(data$Year), FUN = function (y) { if ((y == 2014) | (y == 2020)) 53 else 52 }) # Years with 53 weeks
weeks_y <- cumsum(c(1, n_weeks))
weeks_y[length(weeks_y)] <- weeks_y[length(weeks_y)] - 1


pt <- ggplot(data = data) +
        geom_line(mapping = aes(x = Week_seq, y = Count)) +
        geom_vline(xintercept = weeks_y, color = 'red') + 
        facet_wrap(~ Region, scales = 'free_y', ncol = 4) +
        scale_x_continuous(limits = c(1, max(weeks_y)), expand = c(0, 0),
                           breaks = weeks_y, labels = weeks_y) +
        labs(title = 'Weekly number of Dengue Cases in Brazil between 2014 and 2020',
             x = 'Weeks', y = 'Number of Cases') +
        theme_bw() +
        theme(text = element_text(family = 'LM Roman 10'))

# print(pt)
ggsave(filename = 'brazil_visualization.png', plot = pt, width = 20, height = 14, units = c('in'))
