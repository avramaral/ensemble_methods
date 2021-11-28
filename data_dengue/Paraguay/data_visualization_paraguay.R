setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('tidyverse')

data <- read_csv(file = 'paraguay.csv')

pt <- ggplot(data = data) +
        geom_line(mapping = aes(x = Week_seq, y = Count)) +
        geom_vline(xintercept = c(1, (1:9 * 52)), color = 'red') + 
        facet_wrap(~ Region, scales = 'free_y', ncol = 4) +
        labs(title = 'Weekly number of Dengue Cases in Paraguay between 2012 and 2020',
             x = 'Weeks', y = 'Number of Cases') +
        theme_bw() +
        theme(text = element_text(family = 'LM Roman 10'))

print(pt)
# ggsave(filename = 'paraguay_visualization.png', plot = pt, width = 20, height = 10, units = c('in'))
