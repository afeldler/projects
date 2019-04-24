# Stattleship R Script

players <- players2017
# Subset top 10 stolen base players from players2017 data

topsb <- data.frame()
which(players$first_name == "Dee")
[1] 2257	
which(players$last_name == "Hamilton")
[1] 2433 2434 2435 2436
topsb <- data.frame(players[2257,])
View(topsb)
topsb <- rbind(topsb, players[2433,])
View(topsb)
which(players$first_name == "Trea")
	[1] 6156
topsb <- rbind(topsb, players[6156,])
which(players$last_name == "Pham")
	[1] 4703
topsb <- rbind(topsb, players[4703,])
which(players$last_name == "Reyes")
[1] 5003 5004 5005 5006 5007 5008 5009 5010 5011 5012 5013 5014 5015 5016 5017 5018 5019 5020 5021 5022
[21] 5023 5024 5025
topsb <- rbind(topsb, players[5020,])
which(players$last_name == "Peraza")
[1] 4622
topsb <- rbind(topsb, players[4622,])
which(players$last_name == "Villar")
[1] 6302
topsb <- rbind(topsb, players[6302,])
which(players$last_name == "Inciarte")
[1] 2831
topsb <- rbind(topsb, players[2831,])
which(players$last_name == "Broxton")
[1] 755 756
topsb <- rbind(topsb, players[756,])
which(players$first_name == "Starling")
[1] 3671 4617 4618
which(players$last_name == "Marte")
[1] 3661 3662 3663 3664 3665 3666 3667 3668 3669 3670 3671 3672
topsb <- rbind(topsb, players[3671,])

# Clean up data a little bit

View(topsb)
topsb$created_at <- NULL
View(topsb)
topsb$updated_at <- NULL
View(topsb)
# no players are captains, so remove.
topsb$captain <- NULL
# everyone is weighed in pounds and measured in inches, so remove. 
topsb$unit_of_height <- NULL
topsb$unit_of_weight <- NULL
# ID's taking up space
topsb$league_id <- NULL
topsb$playing_position_id <- NULL
topsb$team_id <- NULL
# draft picks unnecessary for this analysis
topsb$draft_overall_pick <- NULL
topsb$draft_round <- NULL

# Add additional data to data frame to help in performing analysis

topsb$stolen_bases <- c(60, 59, 46, 25, 24, 23, 23, 22, 21, 21) #stolen bases for each player
topsb$weight_kg <- c(77.11, 72.57, 79.38, 79.38, 88.45, 81.65, 97.52, 83.91, 88.45, 83.91) #add column for converted weight in pounds to converted weight in kg
topsb$height_m2 <- c(3.24, 3.35, 3.42, 3.42, 3.42, 3.35, 3.42, 3.17, 3.65, 3.42) #add column for converted height in inches to converted height in meters squared
topsb$weight_kg/topsb$height_m2
[1] 23.79938 21.66269 23.21053 23.21053 25.86257 24.37313 28.51462 26.47003 24.23288 24.53509
topsb$bmi <- topsb$weight_kg/topsb$height_m2 #add column for players bmi
View(topsb)
topsb$OBP <- c(.341, .299, .338, .411, .315, .297, .293, .350, .299, .333) #add column for on-base percentage
topsb$CS <- c(16, 13, 8, 7, 6, 8, 8, 9, 7, 4) #add column for caught stealing
topsb$SB_percentage <- topsb$stolen_bases/(topsb$stolen_bases + topsb$CS) #calculate stolen base percentage and add column
View(topsb)

# Now all of the data is loaded, we can perform some statistical analysis.
bmi <- topsb$bmi
sbases <- topsb$stolen_bases
obp <- topsb$obp 
cs <- topsb$cs
sb_percent <- topsb$SB_percentage
summary(topsb)
# mean height of players is 72.5 in, median height is 73 inches
# mean weight of players is 183.5 lbs, median weight is 182.5 lbs
# mean bmi of players is 24.59, median bmi of players is 24.3
# mean number of stolen bases is 32.4, median number of stolen bases is 23.5

attributes(topsb)
str(topsb)
summary(topsb$bmi)


#Let's visually check out the data.
ggplot(topsb, aes(x = bmi, y = stolen_bases)) + geom_point() + stat_smooth()

ggplot(data=topsb, aes(x=last_name, y=stolen_bases, fill = bmi)) + 
  geom_bar(stat="identity") + xlab("Last name of player") + 
  ylab("Stolen Bases") + ggtitle("Stolen bases of player given BMI") + 
  theme_bw()

scat_plot <- ggplot(topsb, aes(x=last_name, y=CS, color = bmi)) + xlab("Last name of player") + ylab("Caught stealing attempts") + ggtitle("Caught Stealing Attempts of Players by BMI") + geom_point(shape = 21)
scat_plot <- scat_plot + geom_point(shape = 16, size = 5, show.legend = TRUE) +
  theme_minimal()+
  scale_color_gradient(low="plum4", high = "purple1")
scat_plot

ggplot(topsb, aes(x = factor(0), y = stolen_bases)) + 
  geom_boxplot() + xlab("") + ylab("Stolen Bases") +
  scale_x_discrete(breaks = NULL) + coord_flip()
# there does not appear to be any outliers here
ggplot(topsb, aes(x = factor(0), y = bmi)) + 
  geom_boxplot() + xlab("") + ylab("BMI of Players") + 
  scale_x_discrete(breaks = NULL) + coord_flip()
#no outliers here either.

#perform Pearson correlation test.
cor(topsb$bmi, topsb$stolen_bases)
# this correlation coefficient suggests that the level of association between BMI and stolen bases is negatively correlated
#which means that as BMI goes up, stolen bases goes down. 

cor.test(topsb$bmi, topsb$stolen_bases, method = "pearson")
# the p-value of the test is .06542, which is not less than the significance level alpha = 0.05. We cannot conclude that BMI of players and stolen bases are significantly correlated. 

ggscatter(topsb, x = "bmi", y = "stolen_bases", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "BMI", ylab = "Stolen Bases")
# the relationship between the two variables appears to be nonlinear since the scatterplot is showing a curved pattern.
shapiro.test(topsb$bmi)
shapiro.test(topsb$stolen_bases)
#the p-value of BMI is greater than the predetermined significance level of .05 implying that the distribution of this data is normal, however 
#p-value of stolen bases is not greater than .05 so it is not normal.
ggqqplot(topsb$stolen_bases, ylab = "Stolen Bases") #check for normality of stolen bases
ggqqplot(topsb$bmi, ylab = "BMI") #check for normality of BMI


#subset numerical data to create corrplot
topsb1 <- topsb[,30:36]
M <- cor(topsb1)
col <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
corrplot(M, type = "upper", order = "hclust", col = col, tl.col = "darkblue")
# so very few data points have a positive correlation, in fact, most data points are negatively correlated.
# to better visualize
T <- cor_2$r
p_mat <-cor_2$P
corrplot(T, type = "upper", order = "hclust", p.mat = p_mat, sig.level =0.05, insig = "blank")
#almost no metrics are positively correlated - weight in kg is very negatively correlated to stolen bases.....hm

#interesting, let's perform a correlation test between weight in kg and stolen bases
ggscatter(topsb, x = "weight_kg", y="stolen_bases", add = "reg.line", conf.int=TRUE,
          cor.coef = TRUE, cor.method ="pearson", 
          xlab = "Weight in kg", ylab = "Stolen Bases")
# looks pretty highly negatively correlated.
cor(topsb$weight_kg, topsb$stolen_bases)
# the corrrelatioin coefficient is -0.704 which suggests that as weight in kg of player goes up, stolen bases go down.
cor.test(topsb$weight_kg, topsb$stolen_bases, method = "pearson")
# the p-value is 0.02, which is less than the significance level alpha = 0.05. 
# We can conclude that weight in KG of player and stolen bases are significantly correlated with a correlation coefficient of -0.70 and p-value of 0.02. 
#what about height?
ggscatter(topsb, x = "height", y="stolen_bases", add = "reg.line", conf.int=TRUE,
         cor.coef = TRUE, cor.method ="pearson", 
         xlab = "Height", ylab = "Stolen Bases")
cor(topsb$height, topsb$stolen_bases)
cor.test(topsb$height, topsb$stolen_bases)
linMod1 <- lm(stolen_bases~weight_kg, data=topsb)
summary(linMod1)


# Perform linear regression on the data

linMod <- lm(stolen_bases ~ bmi, data = topsb)
linMod
summary(linMod)
attributes(linMod)
#the estimated regression line equation can be written as stolen bases = 155.311 - 4.999*bmi
#the intercept is 155.311. It can be interpreted as the predicted stolen bases for a zero body mass index.
coef(linMod)
plot(linMod)

# plot simple regression line
ggplot(topsb, aes(x=bmi,y=stolen_bases)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
#another way to visualize
ggplot() + geom_point(aes(x=topsb$bmi, y=topsb$stolen_bases),
                      color = "red") +
  geom_line(aes(x=topsb$bmi, y = predict(linMod, newdata = topsb)), 
            color = "blue") +
  ggtitle("BMI vs. Stolen Bases") + xlab("BMI") + ylab("Stolen Bases")

#add 10 additional players to increase sample size
View(topsb2)
plot <- ggplot(topsb2, aes(x=bmi,y=stolen_bases)) + 
  geom_point(color = "pink", size = 4)
plot + geom_line(linetype="solid",size = 1, 
                 color = "black") + 
  geom_label_repel(aes(label=last_name),
                   box.padding = 0.35, point.padding = 0.5, 
                   segment.color = 'grey50') + theme_minimal() + 
  xlab("BMI of player") + ylab("Stolen Bases") + 
  ggtitle("BMI vs. Stolen Bases of Top 20 Players")
#simple linear regression
linMod2 <- lm(stolen_bases ~ bmi, data = topsb2)
summary(linMod2)
#plot linear model
ggscatter(topsb2, x = "bmi", y="stolen_bases", add = "reg.line", conf.int=TRUE,
          cor.coef = TRUE, cor.method ="pearson", 
          xlab = "BMI", ylab = "Stolen Bases")

#correlation plot
R <- cor(topsb3)
corrplot(R, type = "upper", order = "hclust", p.mat = p.mat, sig.level = 0.05)
-----
ggplot(topsb, aes(x=bmi,y=stolen_bases)) +
  geom_point() +
  geom_smooth() +
  xlab('bmi of player')

----
#additional code for analysis:
  
ggplot(topsb, aes(bmi,stolen_bases)) + geom_line()
ggplot(topsb, aes(x=bmi)) + geom_dotplot()
ggplot(topsb, aes(x = position_abbreviation, y = bmi)) + geom_boxplot()

ggplot(topsb, aes(x = bmi, color = position_abbreviation)) + geom_density()
ggplot(topsb, aes(x = position_abbreviation, y = stolen_bases)) + geom_boxplot() + coord_flip()

ggplot(topsb, aes(x=bmi,y=stolen_bases)) + geom_point() + ylab('number of stolen bases')
ggplot(topsb, aes(x=stolen_bases,y=bmi)) + geom_point(position=position_jitter(w=0.1,h=0)) + xlab('number of stolen bases by BMI of player')


xyplot(resid(linMod) ~ fitted(linMod),
xlab = "Fitted Values",
ylab = "Residuals", 
main = "Residual Diagnostic Plot",
panel = function(x,y,...)
{
  panel.grid(h = -1, v = -1)
  panel.abline(h=0)
  panel.xyplot(x,y,...)
}
)

qqmath( ~resid(linMod),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"
)


with(topsb, by(bmi, stolen_bases, median))
