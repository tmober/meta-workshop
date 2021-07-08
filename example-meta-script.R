#####################################################
############## Run meta-analysis in R ###############
#####################################################


############## 1. Set things up ############## 

# Install packages using the pacman package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, googlesheets, metafor, robumeta, ggplot2)


## Import data from Google Sheets
key <- gs_key("1YNw25mC50vlQ1iMYt7d7FgGHCh1b6pZeKHuowFl5BDE")
ef_decod_raw_data <- gs_read(key, ws = "ober_2020_ef_decoding", 
                     col_types = cols(
                       `Study` = col_character(), 
                       `Author` = col_character(), 
                       `PubYear` = col_integer(), 
                       `Country` = col_character(), 
                       `PubPeerRev0` = col_integer(), 
                       `JrnlCiteScore_2018` = col_double(), 
                       `StudyDesign` = col_character(), 
                       `LangEng0` = col_double(), 
                       `WritingSystem` = col_character(), 
                       `MeanAgeYears` = col_double(), 
                       `NSubjects` = col_double(), 
                       `WordDec0` = col_integer(), 
                       `NWordDec0` = col_integer(), 
                       `WorkMem0` = col_integer(), 
                       `TaskSwitch0` = col_integer(), 
                       `InhibCont0` = col_integer(), 
                       `Verbal0NV1` = col_integer(), 
                       `r` = col_double()
                       ), fileEncoding = "UTF-8-BOM") %>%
  as.data.frame()

# Import data from .csv
## Alternatively, you can also download the .csv file, and import it as follows
# read.csv("ober_2020_ef_decoding.csv")


############## 2. Calculate effect sizes ############## 

# Calculate effect size and variances using escalc() function from metafor package

# from correlations (Pearson's r)
ef_decod_data <- escalc(measure = "ZCOR",        # we are generating Fisher's Z-transofrmed correlation values
                    ri = r,                      # the inputted r values
                    ni = NSubjects,              # column containing total sample size
                    data = ef_decod_raw_data,    # dataframe
                    slab = Study)                # study labels


##############  3. Visually inspect data for potential outliers  ############## 

# Check for outliers with violin plot. In this case, the violin plot does not seem to indicate the presence of any obvious outliers  

ggplot(ef_decod_data, aes(x='', y = yi)) + 
  geom_violin(trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.15)) + 
  labs(title="Plot of Effect Size (With Outliers)", x="", y = "Effect size (Fisher's z)")



############## 4. Run the main meta-analytic model ############## 

# Fit the random effects model with no moderators without accounting for study interdependencies
model_metafor <- rma(yi = yi, 
                         vi = vi,
                         data = ef_decod_data,
                         method = "REML",         # restricted maximum likelihood
                         level = 95,              # CI
                         digits = 7,              # decimal points
                         slab = Study)            # study labels

model_metafor

# Fit the random effects model with no moderators accounting for effect size interdependencies using metafor
model_metafor_mv <- rma.mv(yi = yi, 
                           V = vi,
                           data = ef_decod_data,
                           method = "REML",         # restricted maximum likelihood
                           level = 95,              # CI
                           digits = 7,              # decimal points
                           slab = Study,            # study labels
                           random = ~ 1 | Study)    # multilevel model to handle sample dependency (can also use "Author" if it makes more sense to do so)
model_metafor_mv


# Fit the random effects model with no moderators accounting for effect size interdependencies using robumeta
model_robu <- robu(yi ~ 1,
                   var.eff.size = vi, 
                   data = ef_decod_data, 
                   modelweights = "CORR",          # assumes model weights are correlated ("CORR") rather than hierarchical ("HIER"). See Hedges, Tipton and Johnson (2010) and Tipton (2013) for extended explanations of each weighting scheme.
                   studynum = Study,               # study labels
                   small = TRUE)                   # used to fit the meta-regression models with the small- sample corrections for both the residuals and degrees of freedom, as detailed in Tipton (2013).
print(model_robu, digits = 7)



############## 5. Calculate heterogeneity ############## 

# Calculate the I^2 statistic for a multilevel model
# Using the formula from www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

W <- diag(1/ef_decod_data$vi)
X <- model.matrix(model_metafor_mv)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I2_statistic <- 100 * sum(model_metafor_mv$sigma2) / (sum(model_metafor_mv$sigma2) + 
                                                  (model_metafor_mv$k-model_metafor_mv$p)/sum(diag(P)))
I2_statistic


############## 6. Check influential points ############## 

# Using standardized residuals of correlations (and create png)
resid <- residuals(model_metafor_mv) %>%
  scale(center = FALSE, scale = TRUE)  # convert residuals to z-scores

plot(resid, type="p", pch=19)
png(filename = "ResidualsPlot.png", 
    width = 800, height = 640, 
    pointsize = 12, res = 120)
plot(resid, type="p", pch=19)

# Close off set par back to the original settings
# dev.off()


outliers_resid <- resid %>%
  cbind(ef_decod_data$Study) %>%            # bind study names for reference
  subset(resid > 3.0 | resid < - 3.0)       # subset outliers
outliers_resid

# Using Cook's distance
cooks <- cooks.distance(model_metafor_mv)
plot(cooks, type="p", pch=19)
png(filename = "CooksDistancePlot.png", 
    width = 800, height = 640, 
    pointsize = 12, res = 120)
plot(cooks, type="p", pch=19)
# dev.off()

# View outliers with Cooks > 0.5
outliers_cooks <- cooks %>% 
  cbind(ef_decod_data$Study) %>%           # bind study names for reference
  subset(cooks > 0.5)                      # subset outliers
outliers_cooks

# Create new dataframe with outliers removed
ef_decod_data_no_outliers <- ef_decod_data %>%
  cbind(cooks) %>%
  filter(cooks < 3.0*mean(cooks))



############## 7. Create forest plot ############## 

# Create the forest plot
forest(model_metafor_mv, 
       xlim = c(-.5, 1.5),                     # adjust horizontal plot region limits
       order ="obs",                        # order by effect size
       addfit = TRUE,                       # add standard summary polygon
       annotate = TRUE,                     # remove annotations
       width = 0,                           # width of annotations
       efac = .55,                          # height of CI bars
       pch = 19,                            # changing point symbol to filled circle
       col = "gray40",                      # change color of points/CIs
       clim = c(-1 ,1),                     # set absolute limits for CIs
       cex.lab = 1,                         # increase size of x-axis title
       cex.axis = 1,                        # increase size of x-axis labels
       cex = .85,                           # set font expansion factor
       lty = c("solid",                     # CI line type
               "solid",                     # credibility interval line type
               "solid"),                    # horizontal line type
       xlab = "",                           # label X axis
       mlab = "RE Model: p<.001, I2=72.1",  # label summary estimate
       showweights = FALSE,                       # include weights given to effects in model fitting
       steps = 5)                           # number of tick marks for the x-axis

# dev.off()



## Re-run forest plot with median effect size estimates per study  
## Since the previous plot may be difficult to read, we may want to aggregate effect size estimates within each study. The plot below shows only the median effect size for each study. This is not exactly accurate, but does help in creating a simple illustration of the variation in effect sizes between studies.  

## select median effect size and median variance term for each study
ef_decod_data_forest <- ef_decod_data_no_outliers %>%
  select(Study, yi, vi) %>%
  group_by(Study) %>%
  summarise(yi_median = median(yi),
            vi_median = median(vi)) %>%
  ungroup() %>%
  as.data.frame()

## conduct meta-analysis without accounting for clustering
model_metafor_forest <- rma(yi = yi_median, 
                            vi = vi_median,
                            data = ef_decod_data_forest,
                            method = "REML",
                            level = 95,
                            digits = 7,
                            slab = Study)

## create simplified forest plot
forest(model_metafor_forest, 
       xlim = c(-.75, 1.25),                  # adjust horizontal plot region limits
       order ="obs",                          # order by effect size
       addfit = TRUE,                         # add standard summary polygon
       annotate = TRUE,                       # remove annotations
       # width = 0,                           # width of annotations
       # efac = .55,                          # height of CI bars
       # pch = 19,                            # changing point symbol to filled circle
       # col = "gray40",                      # change color of points/CIs
       # clim = c(-1 ,1),                     # set absolute limits for CIs
       # cex.lab = 1,                         # increase size of x-axis title
       # cex.axis = 1,                        # increase size of x-axis labels
       # cex = .85,                           # set font expansion factor
       lty = c("solid",                       # CI line type
               "solid",                       # credibility interval line type
               "solid"),                      # horizontal line type
       xlab = "",                             # label X axis
       mlab = "RE Model: p<.001, I2=68.7",    # label summary estimate
       showweights = FALSE,                   # include weights given to effects in model fitting
       steps = 5)                             # number of tick marks for the x-axis

# dev.off()



############## 8. Run moderator analyses ############## 

# Test moderator 1: (e.g., type of decoding task)
model_moderator1 <-  rma.mv(yi = yi, 
                       V = vi,
                       mods = ~ WordDec0, # allows post hoc tests
                       data = ef_decod_data_no_outliers,
                       random = ~ 1 | Study)
model_moderator1

# Test moderator 2: (e.g., whether verbal or non-verbal stimuli were used)
model_moderator2 <-  rma.mv(yi = yi, 
                      V = vi,
                      mods = ~ Verbal0NV1,
                      data = ef_decod_data_no_outliers,
                      random = ~ 1 | Study)
model_moderator2


############## 9. Test for biases ############## 

# Using published vs unpublished
model_published <- rma.mv(yi = yi, 
                          V = vi,
                          mods = ~ factor(PubPeerRev0), 
                          data = ef_decod_data_no_outliers,
                          random = ~ 1 | Study)
model_published

# Using journal cite score
model_citescore <- rma.mv(yi = yi, 
                          V = vi, 
                        mods = JrnlCiteScore_2018, 
                        data = ef_decod_data_no_outliers,
                        random = ~ 1 | Study)
model_citescore


# Using trim-and-fill analysis
# Need a new model because 'trimfill' doesn't take multivariate models
model_trim_fill <- rma(yi = yi, 
               vi = vi,
               data = ef_decod_data_no_outliers)

# Create funnel plot (with trim and fill)
par(mar=c(4.5,4.5,1,1))
taf <- trimfill(model_trim_fill) # estimate missing studies
funnel(taf,                      # add missing studies to plot
       xlim=c(-.6,1), 
       xlab= "Correlation", 
       ylim = c(.24, 0), 
       steps = 4, 
       digits = c(1, 2))
par(mar=c(2.5,3.6,0,1.5))
# dev.off()
