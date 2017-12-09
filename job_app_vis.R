#devtools::install_github("tidyverse/ggplot2")
library(ggplot2)
Sys.setlocale('LC_TIME', 'English_United Kingdom.1252')
dat = read.table('summary.txt', header = T)

dat[, 4:14] <- as.data.frame(lapply(dat[,4:14], as.Date, format = '%d.%m.%Y'))
dat$end <- as.Date(apply(dat[, 4:14], 1, FUN=max, na.rm = T))

##############################################################################################
# TIME LINE PLOT

p_s = 2.6
p_st = 1.8

p = ggplot() +
  geom_line(data = data.frame(x = c(dat$application_start, dat$end - 1),
                              y = rep(dat$idx, 2)),
            aes(x = x, y = y, group = y),
            colour = 'grey20', size = p_s/2) +
  geom_point(data = dat, aes(x = application_start, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'green4') +
  geom_point(data = dat, aes(x = rejection, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'red3') +
  geom_point(data = dat, aes(x = interview_1 - 0.5, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'darkorange') +
  geom_point(data = dat, aes(x = task_received + 0.5, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'deeppink3') +
  geom_point(data = dat, aes(x = interview_2, y = idx),
             shape = 21, size = p_s, stroke = p_st, colour = 'grey20', fill = 'darkorange') +
  geom_point(data = dat, aes(x = job_offer, y = idx),
             shape = 21, size = p_s *1.5, stroke = p_st*1.5, colour = 'grey20', fill = 'green') +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(caption = '@rikunert', x = 'Sepal length', y = 'Sepal width') +
  theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
  xlab("Date") +
  ylab('Company') +
  ggtitle("Hunting down a data science job")
p

for (i in c(5, 7, 10, 12)){  # for each reaction moment
  p = p + geom_point(data = data.frame(x = dat[, i], y = dat$idx), aes(x = x, y = y),
                     shape = 21, size = p_s/2, stroke = p_st/1.5, colour = 'grey20', fill = 'white')
}
p

legend_fun = function(p, p_text, p_col, p_y){
  p = p + 
    geom_point(data = data.frame(x = c(min(dat$application_start), min(dat$application_start) + 13),
                                 y = c(p_y + 0.6, p_y + 0.6)),
               aes(x = x, y = y), size = 4.6, colour = 'grey20') +
    annotate("rect", xmin = min(dat$application_start), xmax = min(dat$application_start) + 13,
                   ymin = p_y, ymax = p_y + 1.2, fill = 'grey20', colour = 'grey20') +
    annotate('text', x = min(dat$application_start), y = p_y + .65, label=p_text, fontface =2,
             size = 3, color = p_col, hjust = 0)
}

p = legend_fun(p, p_text = 'start', 'green4', 14 + 4)
p = legend_fun(p, p_text = 'interview', 'darkorange', 15.5 + 4)
p = legend_fun(p, p_text = 'task', 'deeppink3', 17 + 4)
p = legend_fun(p, p_text = 'rejection', 'red3', 18.5 + 4)
p = legend_fun(p, p_text = 'offer', 'green', 20 + 4)

p

ggsave('Job_appl_1.png', width = 8.21, height = 4.11, scale = 1, dpi = 1000) # 876 x 438

##############################################################################################
# TRANSITION PLOT

# Transition matrix
stateNames <- c("start","interview 1","task", "interview 2", "rejection", "offer", "no answer", "withdrawal")
m <- matrix(rep(0, length(stateNames)^2),
             nrow = length(stateNames), byrow=TRUE)
row.names(m) <- stateNames
colnames(m) <- stateNames

m['start', 'interview 1'] = sum(!is.na(dat$interview_1))/sum(!is.na(dat$application_start))
m['start', 'no answer'] = sum(dat$application_start == dat$end)/sum(!is.na(dat$application_start))
m['start', 'rejection'] = sum(is.na(dat$interview_1) & !is.na(dat$rejection))/sum(!is.na(dat$application_start))

m['interview 1', 'task'] = sum(!is.na(dat$interview_1) & !is.na(dat$task_received))/sum(!is.na(dat$interview_1))
m['interview 1', 'interview 2'] = sum(!is.na(dat$interview_1) & is.na(dat$task_received) & !is.na(dat$interview_2))/sum(!is.na(dat$interview_1))
m['interview 1', 'rejection'] = sum(!is.na(dat$interview_1) & is.na(dat$task_received) & is.na(dat$interview_2) & !is.na(dat$rejection))/sum(!is.na(dat$interview_1))
m['interview 1', 'withdrawal'] = sum(!is.na(dat$interview_1) & is.na(dat$task_received) & is.na(dat$rejection) & is.na(dat$job_offer))/sum(!is.na(dat$interview_1))

m['task', 'interview 2'] = sum(!is.na(dat$task_received) & !is.na(dat$interview_2))/sum(!is.na(dat$task_received))
m['task', 'withdrawal'] = sum(!is.na(dat$task_received) & is.na(dat$rejection) & is.na(dat$job_offer) & is.na(dat$interview_2))/sum(!is.na(dat$task_received))

m['interview 2', 'rejection'] = sum(!is.na(dat$interview_2) & !is.na(dat$rejection))/sum(!is.na(dat$interview_2))
m['interview 2', 'offer'] = sum(!is.na(dat$interview_2) & !is.na(dat$job_offer))/sum(!is.na(dat$interview_2))
m['interview 2', 'withdrawal'] = sum(!is.na(dat$interview_2) & is.na(dat$rejection) & is.na(dat$job_offer))/sum(!is.na(dat$interview_2))

m

dat_trans = data.frame(labels = c('start', 'interview 1', 'task', 'interview 2', 'offer', 'withdrawal', 'no answer', 'rejection'),
                       x = c(1, 2, 3, 4, 5, 3, 1, 2.5),
                       y = c(3, 3, 3, 3, 3, 4.5, 1.5, 1),
                       s = c(sum(!is.na(dat$application_start)), sum(!is.na(dat$interview_1)), sum(!is.na(dat$task_received)),
                             sum(!is.na(dat$interview_2)), sum(!is.na(dat$job_offer)),
                             sum(!is.na(dat$interview_1) & is.na(dat$rejection) & is.na(dat$job_offer)),
                             sum(!is.na(dat$application_start) & is.na(dat$interview_1) & is.na(dat$rejection) & is.na(dat$job_offer)),
                             sum(!is.na(dat$rejection))))
#levels(dat_trans$labels) = c('start', 'interview 1', 'task', 'interview 2', 'offer', 'withdrawal', 'no answer', 'rejection')
dat_trans

cols = c('start' = 'green4', 'interview 1' = 'darkorange', 'task' = 'deeppink3', 'interview 2' = 'darkorange',
         'offer' = 'green', 'withdrawal' = 'white', 'no answer' = 'red3', 'rejection' = 'red3')

p_s = 2.6
p_st = 1.8

p = ggplot()

# arrows
arrow_fun = function(p, x1, x2, y1, y2, s, curve = 0){
  p = p + geom_curve(data = data.frame(x1 = x1, x2 = x2, y1 = y1, y2 = y2),
                     aes(x = x1, y = y1, xend = x2, yend = y2), curvature = curve,
                     arrow = arrow(length = unit(0.03, "npc")),
                     size = s, color = 'grey20')
}
a_c = 8
p = arrow_fun(p, 1, 2 - 0.22, 3, 3, a_c * m['start', 'interview 1'])  # start -> interview 1
p = arrow_fun(p, 2, 3 - 0.17, 3, 3, a_c * m['interview 1', 'task'])  # interview 1 -> task
p = arrow_fun(p, 3, 4 - 0.25, 3, 3, a_c * m['task', 'interview 2'])  # task -> interview 2
p = arrow_fun(p, 4, 5 - 0.09, 3, 3, a_c * m['interview 2', 'offer'])  # interview 2 -> offer
p = arrow_fun(p, 2, 4-0.15, 2.98, 2.78, a_c * m['interview 1', 'interview 2'], 0.4)  # interview 1 -> interview 2
p = arrow_fun(p, 1, 1, 3, 1.9, a_c * m['start', 'no answer'])  # start -> no answer
p = arrow_fun(p, 1, 2.2, 3, 1, a_c * m['start', 'rejection'], 0.3)  # start -> rejection
p = arrow_fun(p, 2, 2.35, 3, 1.5, a_c * m['interview 1', 'rejection'], 0)  # interview 1 -> rejection
p = arrow_fun(p, 4, 2.8, 3, 1, a_c * m['interview 2', 'rejection'], -0.3)  # interview 2 -> rejection
p = arrow_fun(p, 2, 2.85, 3, 4.33, a_c * m['interview 1', 'withdrawal'], 0)  # interview 1 -> withdrawal
p = arrow_fun(p, 4, 3.15, 3, 4.33, a_c * m['interview 2', 'withdrawal'], 0)  # interview 2 -> withdrawal

p

# state labels
legend_fun2 = function(p, p_text, p_col,p_x, p_y){
  p = p + 
    geom_point(data = data.frame(x = c(p_x, p_x + 0.4),
                                 y = c(p_y + 0.077, p_y + 0.077)),
               aes(x = x, y = y), size = 3.6, colour = 'grey20') +
    annotate("rect", xmin = p_x, xmax = p_x + 0.4,
             ymin = p_y, ymax = p_y + 0.15, fill = 'grey20', colour = 'grey20') +
    annotate('text', x = p_x, y = p_y + .08, label=p_text, fontface =2,
             size = 3, color = p_col, hjust = 0)
}

p1 = legend_fun2(p, p_text = 'start', cols['start'], 0.5, 3.22)
p1 = legend_fun2(p1, p_text = 'interview 1', cols['interview 1'], 1.45, 3.22)
p1 = legend_fun2(p1, p_text = 'task', cols['task'], 2.8, 3.22)
p1 = legend_fun2(p1, p_text = 'interview 2', cols['interview 2'], 3.53, 3.22)
p1 = legend_fun2(p1, p_text = 'offer', cols['offer'], 4.9, 3.22)
p1 = legend_fun2(p1, p_text = 'no answer', cols['no answer'], 0.45, 1.2)
p1 = legend_fun2(p1, p_text = 'rejection', cols['rejection'], 1.9, 0.7)
p1 = legend_fun2(p1, p_text = 'withdrawal', cols['withdrawal'], 2.55, 4.7)

p1

# state labels
trans_legend_fun = function(p, p_text,p_x, p_y){
  p = p + 
    annotate("rect", xmin = p_x, xmax = p_x + 0.25,
             ymin = p_y-0.05, ymax = p_y + 0.2, fill = 'white', colour = 'grey20', size = 1) +
    annotate('text', x = p_x + 0.02, y = p_y + .08, label=p_text, fontface =2,
             size = 3, color = 'black', hjust = 0)
}

p2 = trans_legend_fun(p1, p_text = sprintf('%d%%', round(m['start', 'interview 1'] * 100)), 1.4, 2.93)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 1', "task"] * 100)), 2.2, 2.93)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m["task", "interview 2"] * 100)), 3.15, 2.93)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 2', "offer"] * 100)), 4.2, 2.93)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['start', "no answer"] * 100)), 0.88, 2.1)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['start', "rejection"] * 100)), 1.2, 1.8)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 1', "rejection"] * 100)), 2, 2.35)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 1', "interview 2"] * 100)), 2.3, 2.4)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 1', "withdrawal"] * 100)), 2.2, 3.5)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 2', "withdrawal"] * 100)), 3.5, 3.5)
p2 = trans_legend_fun(p2, p_text = sprintf('%d%%', round(m['interview 2', "rejection"] * 100)), 3.75, 2.4)

# dots and theme specs
minSize = 1
maxSize = 30
p3 = p2 + geom_point(data = dat_trans, aes(x = x, y = y, fill = labels, size = s),
           shape = 21, stroke = p_st, colour = 'grey20') +
  scale_size_continuous(range = c(minSize, maxSize)) +
  scale_fill_manual(values = cols) +
  theme_classic() +
  labs(caption = '@rikunert') +
  theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic')) +
  theme(legend.position = 'none', axis.line=element_blank(), axis.text=element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank()) +
  ggtitle("The data science application funnel")

p3

ggsave('Job_appl_2.png', width = 8.21, height = 4.11, scale = 1, dpi = 1000) # 876 x 438