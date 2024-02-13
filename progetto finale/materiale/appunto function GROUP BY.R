#this function doesn't work...

# variable_conditional_summary = function(df,variable,conditional){
#     return(df %>% group_by(conditional) %>% summarize(n=n(),
#                                                mean = mean(variable),
#                                                sigma = sd(variable),
#                                                CV = sigma/mean * 100,
#                                                iqr = IQR(variable),
#                                                min = min(variable),
#                                                Q1 = quantile(variable,0.25,names=F),
#                                                median = median(variable),
#                                                Q3 = quantile(variable,0.75,names = F),
#                                                max = max(variable)))
# } 