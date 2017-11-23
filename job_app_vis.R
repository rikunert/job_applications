install.packages('ggplot2')
library(ggplot2)
dat = read.table('summary.txt', header = T)

dat[, 4:14] <- as.data.frame(lapply(dat[,4:14], as.Date, format = '%d.%m.%Y'))
dat$end <- apply(dat[, 4:14], 1, FUN=max, na.rm = T)

p_s = 2.6
p_st = 1.8

p = ggplot() +
  geom_point(data = dat, aes(x = application_start, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'green4') +
  geom_point(data = dat, aes(x = rejection, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'red3') +
  geom_point(data = dat, aes(x = interview_1, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'darkorange') +
  geom_point(data = dat, aes(x = task_received, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'deeppink3') +
  geom_point(data = dat, aes(x = interview_2, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'darkorange') +
  geom_point(data = dat, aes(x = job_offer, y = idx),
             shape = 21, size = p_s *1.5, stroke = p_st*1.5, colour = 'grey20', fill = 'green') +
  scale_y_reverse() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(caption = '@rikunert', x = 'Sepal length', y = 'Sepal width') +
  theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
  xlab("Date") +
  ylab('Company') +
  ggtitle("Data Science applications")
p

for (i in c(5, 7, 10, 12)){  # for each raction moment
  p = p + geom_point(data = data.frame(x = dat[, i], y = dat$idx), aes(x = x, y = y),
                     shape = 21, size = p_s/2, stroke = p_st/1.5, colour = 'grey20', fill = 'white')
}
p

legend_fun = function(p, p_text, p_col, p_y){
  p = p + 
    geom_point(data = data.frame(x = c(min(dat$application_start), min(dat$application_start) + 12),
                                 y = c(p_y + 0.6, p_y + 0.6)),
               aes(x = x, y = y), size = 4.6, colour = 'grey20') +
    annotate("rect", xmin = min(dat$application_start), xmax = min(dat$application_start) + 12,
                   ymin = p_y + 1.2, ymax = p_y, fill = 'grey20', colour = 'grey20') +
    annotate('text', x = min(dat$application_start), y = p_y + .5, label=p_text, fontface =2,
             size = 3, color = p_col, hjust = 0)
}

p = legend_fun(p, p_text = 'start', 'green4', 14)
p = legend_fun(p, p_text = 'interview', 'darkorange', 15.5)
p = legend_fun(p, p_text = 'task', 'deeppink3', 17)
p = legend_fun(p, p_text = 'rejection', 'red3', 18.5)
p = legend_fun(p, p_text = 'offer', 'green', 20)

p

ggsave('Job_appl_1.png', width = 8.21, height = 4.11, scale = 1, dpi = 1000) # 876 x 438


# for (i in 1:dim(dat)[2]){
#   # annotate("segment", x = as.numeric(ymd(20130401)), xend = as.numeric(ymd(20130701)), 
#   #          y = -10, yend = 10)
#   p = p + annotate('segment', x = dat$application_start[i],
#                    y = dat$idx[i], 
#                    xend = dat$end[i], 
#                    yend = dat$idx[i],
#            colour = 'grey',
#            size = 2)
# }
# p
# 
# 
# 
# 
# tu <- expand.grid(Land       = gl(2, 1, labels = c("DE", "BB")),
#                   Altersgr   = gl(5, 1, labels = letters[1:5]),
#                   Geschlecht = gl(2, 1, labels = c('m', 'w')),
#                   Jahr       = 2000:2009)