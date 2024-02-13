# The report about the final project is available at the following drive link:
# https://drive.google.com/drive/folders/1eE0w9AvyXeZiJhg8VpWA0qWuyQbR9sMZ?usp=sharing
# In the same folder there are some csv files, used to create tables in the google doc.

rm(list= ls())
library(ggplot2)
library(ggthemes)
library(moments)
library(colorspace)
library(scales)
library(dplyr)

#Change the working directory where you have the Real Estate Texas.csv file.
setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-descrittiva-per-datascience/progetto finale")

#1.
real_estate_data = read.csv("Real Estate Texas.csv",sep=",")
attach(real_estate_data)


#2.

##VARIABLE TYPES
#City: qualitative - nominal scale
#Year: qualitative - ordinal scale
#Month: qualitative - ordinal scale
#Sales. quantitative - discrete (int values)
#Volume: quantitative - continuous
#Median_price: quantitative - continuous
#Listings: quantitative - discrete (int values)
#Months_inventory: quantitative - continuous

#3.

#use a custom function to compute position indexes (quantitative vars)
position.summary = function(x) {
    quartiles = quantile(x)
    x_min = quartiles[1]
    x_Q1 = quartiles[2]
    x_median = quartiles[3]
    x_Q3 = quartiles[4]
    x_max = quartiles[5]
    
    d = data.frame(min = x_min, Q1 = x_Q1, median = x_median, Q3 = x_Q3, max = x_max)
    return(d)
}

#use a custom function to compute variability indexes (quantitative vars)
variability.summary = function(x) {
    n = length(x)
    x_mu = mean(x)
    x_IQR = IQR(x)
    x_var = var(x)
    x_sigma = sqrt(x_var)
    x_CV = x_sigma/x_mu * 100 #coefficient of variability
    
    d = data.frame(n = n, mu=x_mu,variance = x_var,sigma = x_sigma, CV = x_CV)
    return(d)
}

#use a custom function to compute shape indexes (quantitative vars)
shape.summary = function(x){
    x_skewness = skewness(x)
    x_kurtosis = kurtosis(x)-3
    
    d = data.frame(skewness = x_skewness, kurtosis = x_kurtosis)
    return(d)
}

#custom function to save all statistical indexes for a quantitative variable
complete.summary = function(x) {
    var_name = deparse(substitute(x))
    
    df_position = position.summary(x)
    df_variability = variability.summary(x)
    df_shape = shape.summary(x)
    total_summary = cbind(data.frame(name = var_name),
                          df_position,df_variability,df_shape)
    write.csv(total_summary,paste0(var_name, " stats.csv"),row.names = F)
    return(total_summary)
    
}

sales_summary = complete.summary(sales)
volume_summary = complete.summary(volume)
median_price_summary = complete.summary(median_price)
listings_summary = complete.summary(listings)
months_inventory_summary = complete.summary(months_inventory)

##qualitative variables

##custom function to compute frequency distributions
frequency.distributions = function(x,save_as_csv=F) {
    N = length(x)
    absolute = table(x) #absolute frequency distr.
    relative = absolute/N #relative frequency distr.
    cumulative = cumsum(absolute) #cumulative frequency distr.
    cumulative_rel = cumulative/N #relative cumulative frequency distr.
    
    distribution = cbind(absolute,relative,cumulative,cumulative_rel)
    
    d = as.data.frame(distribution)
    if(save_as_csv) {
        var_name = deparse(substitute(x))
        write.csv(d,paste0(var_name, " stats.csv"),row.names = T)
    }
    
    return(d)
}

city_freq_distributions = frequency.distributions(city,T)
months_freq_distributions = frequency.distributions(month,T)
year_freq_distributions = frequency.distributions(year,T)

#remove these charts
city_factor = as.factor(city)
ggplot(data=real_estate_data)+
    geom_bar(aes(x=city_factor),stat = "count",
             color = "black",fill="darkolivegreen2")+
    labs(title="City absolute frequency distribution", x = "City", y = "Frequency")


month_factor = as.factor(month)
ggplot(data=real_estate_data)+
    geom_bar(aes(x=month_factor),stat = "count",
             col = "black", fill="aquamarine4")+
    scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
    labs(title="Month absolute frequency distribution", x = "Month", y = "Frequency")

year_factor = as.factor(year)
ggplot(data=real_estate_data)+
    geom_bar(aes(x=year_factor),stat = "count",
             color = "black", fill = "lightsteelblue4")+
    labs(title="Year absolute frequency distribution", x = "Year", y = "Frequency")


#4.
#Variable with highest variability is volume, because it has the highest
#CV (coefficient of variability) among all variables.
#Volume is the most asymmetric variable too, because it has the highest
#skewness value (in module); it has a positive asymmetry.

#5.
#Variable listings

#listings_cl = cut(listings,breaks = 10, dig.lab = 5)
listings_cl = cut(listings,breaks = 10, dig.lab = 4)

listings_freq_distr = frequency.distributions(listings_cl)

#plot absolute frequency distribution

class_names_sorted = factor(rownames(listings_freq_distr),levels = rownames(listings_freq_distr))

#absolute
ggplot(data =listings_freq_distr, aes(x=class_names_sorted,y=absolute))+
    geom_col(col="black",fill="lemonchiffon")+ #gray77 - aliceblue
    labs(title="Listings absolute frequency distribution",x = "Listings (class)",y = "Frequency")+
    scale_y_continuous(breaks = seq(0,70,10))+
    geom_text(aes(label=absolute),vjust=-0.5)+
    theme_hc()
# ggsave(filename = "listings absolute freq.png")

#relative
ggplot(data =listings_freq_distr, aes(x=class_names_sorted,y=relative))+
    geom_col(col="black",fill="lemonchiffon")+
    labs(title="Listings relative frequency distribution",x = "Listings (class)",y = "Frequency")+
    # scale_y_continuous(breaks = seq(0,1,0.1))+
    scale_y_continuous(labels = label_percent())+
    geom_text(aes(label=percent(relative)),vjust=-0.5)+
    theme_hc()
# ggsave(filename = "listings relative freq.png")

#cumulative
ggplot(data =listings_freq_distr, aes(x=class_names_sorted,y=cumulative))+
    geom_col(col="black",fill="lemonchiffon")+
    labs(title="Listings cumulative frequency distribution",x = "Listings (class)",y = "Frequency")+
    scale_y_continuous(breaks = seq(0,240,40))+
    geom_text(aes(label=cumulative),vjust=-0.5)+
    theme_hc()
# ggsave(filename = "listings cumulative freq.png")

#relative cumulative
ggplot(data =listings_freq_distr, aes(x=class_names_sorted,y=cumulative_rel))+
    geom_col(col="black",fill="lemonchiffon")+
    labs(title="Listings relative cumulative frequency distribution",x = "Listings (class)",y = "Frequency")+
    # scale_y_continuous(breaks = seq(0,70,10))+
    scale_y_continuous(labels = label_percent())+
    geom_text(aes(label=percent(cumulative_rel)),vjust=-0.5)+
    theme_hc()
# ggsave(filename = "listigns relative cumulative freq.png")

#gini index
gini.index <- function(x){
    ni = table(x)
    fi = ni/length(x)
    fi2 = fi^2
    J = length(table(x))
    
    gini = 1-sum(fi2)
    gini.norm = gini/((J-1)/J)
    
    return(gini.norm)
}

gini_listings = gini.index(listings_cl)
# 0.924 very close to 1 -> the variable is almost equally distributed

#6.
#City variable has Gini index = 1 because it's equally distributed. 
#It's confirmed by the frequency distributions computed earlier.
#However, we can compute the index with our custom function:

gini_city = gini.index(city)

#7.
#To answer to this question we can compute the mass probability function
#of the variable city, and then check the value for the value "Beaumont".
#According to classical probability definition, the probability function, in this case,
#is equal to the relative frequency distribution.

# city_freq_distributions = frequency.distributions(city)

ggplot(data=city_freq_distributions)+
    geom_col(aes(x=rownames(city_freq_distributions),y=relative),
             col = "lemonchiffon4",fill="salmon4")+
    ggtitle("Mass probability function for city variable")+
    xlab("City")+
    ylab("Probability")+
    scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1),
                       labels=percent_format(accuracy=1))+
    #geom_text(aes(x=rownames(city_freq_distributions),y=relative,label=relative))+
    theme_hc()
# ggsave(filename = "city mass probability function.png")

#As one can see, the function is uniform, and is equal to 0.25. 
#So the probability to extract Beaumont from the dataset is 25%.

#Same for the variable month and july value.

# months_freq_distributions = frequency.distributions(month)

months_names_sorted = factor(rownames(months_freq_distributions),
                             labels =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(data=months_freq_distributions)+
    geom_col(aes(x=months_names_sorted,y=relative),
             col = "honeydew4",fill="royalblue4")+
    ggtitle("Mass probability function for months variable")+
    xlab("Months")+
    ylab("Probability")+
    scale_y_continuous(breaks=seq(0,1,0.25),limits = c(0,1),
                       labels=percent_format(accuracy=1))+
    theme_hc()
# ggsave(filename = "month mass probability function.png")

#The probability to extract July is 8.33%.

#Probability of december 2012: one can see this event as: what is the chance
#to extract december AND 2012? In statistical terms this means to compute the
#joint probability between the event "extract month december" and "extract year 2012".
#These two variables are independent (no variable provides any information about
#the other one), so the joint probability is:

#p(month = december, year = 2012) = p(month = december) * p(year = 2012)

# year_freq_distributions = frequency.distributions(year)

p_dec_2012 = months_freq_distributions["12","relative"] * year_freq_distributions["2012","relative"] 

#8.

#to compute mean price, one can use volume and sales, since:
# - sales = N, where N is the total sales number.
# - volume = total sales value, so it's sum(sale_values) = sum(price).
# Because volume is in millions of dollars, one can multiply this value for 1e6
# in order to convert in dollars.

real_estate_data$mean_price = 1e6*volume/sales #in dollars

#9.
#Effectiveness of sales announcements.

#A measure of effectiveness could be the ratio between sales and listings.
#This value is between 0 and 1, where 0 means no real estate was sold, and
#1 means all announcements have been completed.
#This parameter is related more to the agent's ability rather than market value.
#One can use this parameter to compare sales among cities and periods where the
#market has different values (in terms of economical volumes and/or sales 
#announcements density).
real_estate_data$effectiveness = sales/listings
attach(real_estate_data)

#just for curiosity...
effectiveness_cl = cut(effectiveness, breaks = seq(0,0.5,0.05))

freq_dist_eff = frequency.distributions(effectiveness_cl)

ggplot(data=freq_dist_eff,aes(x=rownames(freq_dist_eff),y=relative))+
    geom_col(fill="tomato")+
    geom_text(aes(label=relative),vjust=-0.5)+
    xlab("class")+
    ylab("count")+
    ggtitle("Freq distribution")+
    theme_hc()

#10.

#listings
# listings_by_city_summary_V2 = variable_conditional_summary(real_estate_data,listings,city)
listings_by_city_summary = real_estate_data %>% group_by(city) %>% summarize(n=n(),
                                                                             mean = mean(listings),
                                                                             sigma = sd(listings),
                                                                             CV = sigma/mean * 100,
                                                                             iqr = IQR(listings),
                                                                             min = min(listings),
                                                                             Q1 = quantile(listings,0.25,names=F),
                                                                             median = median(listings),
                                                                             Q3 = quantile(listings,0.75,names = F),
                                                                             max = max(listings),
                                                                             total = sum(listings))

listings_by_year_summary = real_estate_data %>% group_by(year) %>% summarize(n=n(),
                                                                             mean = mean(listings),
                                                                             sigma = sd(listings),
                                                                             CV = sigma/mean * 100,
                                                                             iqr = IQR(listings),
                                                                             min = min(listings),
                                                                             Q1 = quantile(listings,0.25,names=F),
                                                                             median = median(listings),
                                                                             Q3 = quantile(listings,0.75,names = F),
                                                                             max = max(listings),
                                                                             total = sum(listings))

listings_by_month_summary = real_estate_data %>% group_by(month) %>% summarize(n=n(),
                                                                               mean = mean(listings),
                                                                               sigma = sd(listings),
                                                                               CV = sigma/mean * 100,
                                                                               iqr = IQR(listings),
                                                                               min = min(listings),
                                                                               Q1 = quantile(listings,0.25,names=F),
                                                                               median = median(listings),
                                                                               Q3 = quantile(listings,0.75,names = F),
                                                                               max = max(listings),
                                                                               total = sum(listings))

#volume
volume_by_city_summary = real_estate_data %>% group_by(city) %>% summarize(n=n(),
                                                                           mean = mean(volume),
                                                                           sigma = sd(volume),
                                                                           CV = sigma/mean * 100,
                                                                           iqr = IQR(volume),
                                                                           min = min(volume),
                                                                           Q1 = quantile(volume,0.25,names=F),
                                                                           median = median(volume),
                                                                           Q3 = quantile(volume,0.75,names = F),
                                                                           max = max(volume),
                                                                           total = sum(volume))

volume_by_year_summary = real_estate_data %>% group_by(year) %>% summarize(n=n(),
                                                                           mean = mean(volume),
                                                                           sigma = sd(volume),
                                                                           CV = sigma/mean * 100,
                                                                           iqr = IQR(volume),
                                                                           min = min(volume),
                                                                           Q1 = quantile(volume,0.25,names=F),
                                                                           median = median(volume),
                                                                           Q3 = quantile(volume,0.75,names = F),
                                                                           max = max(volume),
                                                                           total = sum(volume))

volume_by_month_summary = real_estate_data %>% group_by(month) %>% summarize(n=n(),
                                                                             mean = mean(volume),
                                                                             sigma = sd(volume),
                                                                             CV = sigma/mean * 100,
                                                                             iqr = IQR(volume),
                                                                             min = min(volume),
                                                                             Q1 = quantile(volume,0.25,names=F),
                                                                             median = median(volume),
                                                                             Q3 = quantile(volume,0.75,names = F),
                                                                             max = max(volume),
                                                                             total = sum(volume))

#useful for the report
write.csv(listings_by_city_summary,"listings by city summary.csv",row.names = F)
write.csv(volume_by_city_summary,"volume by city summary.csv",row.names = F)

## ggplot2 section

#g1.

#boxplot of median_price vs city.

ggplot(data=real_estate_data, aes(x=city,y=median_price))+
    geom_boxplot(col="black",fill="gray85")+
    ggtitle("Boxplot median price vs city")+
    xlab("City")+
    ylab("Median price ($)")+
    theme_hc()

# ggsave(filename = "g1.png")

#Bryan-College Station has the most expensive houses.
#Wichita Falls has the cheapest houses, and moreover it has the widest
#choice, as its interquartile range is the greatest among cities.

#g2.
# boxplot of volume vs city and year

ggplot(data=real_estate_data, aes(x=city,y=volume))+
    geom_boxplot(aes(fill=year_factor))+
    ggtitle("Boxplot volume vs city")+
    xlab("City")+
    ylab("Volume (million of $)")+
    scale_fill_discrete_sequential(palette="red-yellow")+ #Teal - TealGrn - red-yellow
    labs(fill="Year")+
    theme_hc()

# ggsave(filename = "g2.png")

#At Wichita Falls economy is at a standstill.
#Instead, Tyler is the best one, with the greatest growth.
#Other two cities have grown too, but slower.

#g3.
#stacked bar plot, of volume vs city and year

ggplot(data=real_estate_data,aes(x=city,y=volume))+
    geom_col(aes(fill=year_factor),position = "stack")+
    ggtitle("Total volume per city")+
    xlab("City")+
    ylab("Volume (million of $)")+
    scale_fill_discrete_sequential(palette="blues 2")+ #light grays
    labs(fill="Year")+
    scale_y_continuous(breaks = seq(0,2500,500))+
    theme_hc()

# ggsave("g3.png")

ggplot(data=real_estate_data,aes(x=city,y=volume))+
geom_col(aes(fill=year_factor),position = "fill")+
    ggtitle("Stacked normalized total volume per city")+
    xlab("City")+
    ylab("Volume percentage")+
    scale_fill_discrete_sequential(palette="reds 2")+ #light grays
    labs(fill="Year")+
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels=percent_format(accuracy=1))+
    theme_hc()
# ggsave("g3 normalized.png")
        
#g4.
#plot volume in years for several cities.

volume_by_year_cities = real_estate_data %>% group_by(year,city) %>% summarise(total = sum(volume),
                                                                               n=n())

ggplot(data = volume_by_year_cities)+
    geom_line(aes(x = year, y = total,color=city),lwd=1)+
    scale_color_discrete_qualitative(palette="dark3")+
    ggtitle("Total volume over years for cities")+
    ylab("Volume (million of $)")+
    xlab("Year")+
    labs(color="City")+
    theme_hc()

# ggsave(filename = "g4.png")

#These two last graphs confirm the previous comments.





