#ggplot2 
#install.packages("ggplot2")

if(!require(ggplot2)){
  install.packages('ggplot')
  library(ggplot2)
} else {
    library(ggplot2)
}

# Check if file exists
if(!file.exists('Mohammad-Omar-Es_Haqzai-itc255.csv')){
  stop("File 'Mohammad-Omar-Es_Haqzai-itc255.csv' not found in working directory")
}

#Read the dataset
dtSurvey=read.csv('Mohammad-Omar-Es_Haqzai-itc255.csv')
head(dtSurvey)
View(dtSurvey)

#Check the structure of the data
str(dtSurvey)

# Set factor levels for Satisfaction to have logical order
dtSurvey$Satisfaction <- factor(dtSurvey$Satisfaction, 
                                levels = c("Not Satisfied", "Satisfied", "Very Satisfied"))

head(dtSurvey)


#univar gaphs
##++++++++++++++++++Pie chart
#Gender

fdtGender=table(dtSurvey$Gender)
fdtGender=as.data.frame(fdtGender)
colnames(fdtGender)=c("Gender", "Count")
fdtGender


##we use the FDT of Gender as input for ggplot
g0=ggplot(fdtGender, aes(x = "", y = Count, fill = Gender))
g1=g0 + geom_col() +
  coord_polar(theta = "y") +
  theme_void() +
  theme(plot.title = element_text(colour = "blue",
                                  size = 12, 
                                  face = "bold", 
                                  hjust = .5)) +
  ggtitle('Gender Distribution of Survey Respondents') +
  geom_text(aes(label = Count), 
            position = position_stack(vjust = .5)) +
  scale_fill_manual(values = c('#99FF33', '#BE2A3E')) +
  theme(legend.position = 'bottom')

ggsave('genderDist.png')
g1


###++++++++++++++++++++++ Bar Chart - Gender
g0=ggplot(fdtGender, aes(x = Gender, y = Count, fill = Gender))
g0 + geom_bar(stat = 'identity') +
  theme_classic() +
  theme(legend.position = '') +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust = .5)) +
  ggtitle('Survey Respondents Gender Distribution') +
  geom_text(aes(label = Count), vjust = 2) +
  scale_fill_manual(values = c('#FF9933', '#0000CC'))
ggsave('genderBar.pdf')

###++++++++++++++++++++++ Bar Chart - Satisfaction
fdtSatisfaction=table(dtSurvey$Satisfaction)
fdtSatisfaction=as.data.frame(fdtSatisfaction)
colnames(fdtSatisfaction)=c('Satisfaction', 'Count')

g0=ggplot(fdtSatisfaction, aes(x = Satisfaction, y = Count, fill = Satisfaction))
g0 + geom_bar(stat = 'identity') +
  theme_classic() +
  theme(legend.position = '') +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust = .5)) +
  ggtitle('Satisfaction Level Distribution') +
  geom_text(aes(label = Count), vjust = 2) +
  scale_fill_brewer(palette = "Set2")
ggsave('satisfactionBar.png')


###++++++++++++++++++++++++++ Histogram - Age
g0=ggplot(dtSurvey, aes(x = Age))
g0 + geom_histogram(bins = 10, fill = '#99FFFF', colour = 4) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title.x = element_text(), 
        axis.title.y = element_text()) +
  ggtitle('Age Distribution of Respondents') +
  xlab('Age') +
  ylab('Frequency') +
  geom_vline(xintercept = mean(dtSurvey$Age),
             linetype = 'dashed',
             color = 'red', 
             size = 1) +
  geom_vline(xintercept = median(dtSurvey$Age),
             linetype = 'dashed',
             color = 'blue', 
             size = 1)
ggsave('ageDistHist.png')



###++++++++++++++++++++ Density plot - Age
g0=ggplot(dtSurvey, aes(x = Age))
g0 + geom_density(color = 'red', fill = 'pink', alpha = 0.3, size = 1) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text()) +
  ggtitle('Age Distribution Density') +
  xlab('Age') +
  ylab('Density') +
  geom_vline(xintercept = mean(dtSurvey$Age),
             linetype = 'dashed',
             color = 'blue', 
             size = 1)
ggsave('ageDensity.png')


## ECDF - Age
g0=ggplot(dtSurvey, aes(x = Age))
g1=g0 + stat_ecdf(geom = "step", 
                     col = "red", size = 1) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text()) +
  ggtitle('Age Cumulative Distribution') +
  xlab('Age') +
  ylab('Cumulative Probability') +
  geom_vline(xintercept = 30,
             linetype = 'dashed',
             color = 'blue', 
             size = 1) +
  geom_hline(yintercept = ecdf(dtSurvey$Age)(30),
             linetype = 'dashed',
             color = 'blue', 
             size = 1)

g1
ggsave('ageECDF.png')

# Check the ECDF value at age 30
ecdf(dtSurvey$Age)(30)


###+++++++++++++++++++ Box Plot - Height
g0=ggplot(dtSurvey, aes(y = '', x = Height))
g0 + geom_boxplot(fill = 'orange', 
                  color = 'brown', 
                  alpha = 0.3, 
                  outlier.colour = 'red', 
                  linetype = 2, 
                  lwd = .6) +
  theme_classic() +
  theme(axis.title.x = element_text(), 
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color = 'darkgreen')) +
  ggtitle('Box Plot of Height Distribution') +
  xlab('Height (cm)') +
  ylab('')
ggsave('boxplotHeight.png')
