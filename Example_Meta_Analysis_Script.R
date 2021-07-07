##########################################################################
############## Run meta-analysis using the metafor package ###############
##########################################################################


############## 1. Set things up ############## 

# Install packages using the pacman package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets, metafor, robumeta, ggplot2)


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
                       )) %>%
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


############## 3. Run the main meta-analytic model ############## 

# Fit the random effects model with no moderators without accounting for study interdependencies
model_metafor <- rma(yi = yi, 
                         V = vi,
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



############## 4. Calculate heterogeneity ############## 

# Calculate the I^2 statistic for a multilevel model
# Using the formula from www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

W <- diag(1/ef_decod_data$vi)
X <- model.matrix(model_metafor_mv)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I2_statistic <- 100 * sum(model_metafor_mv$sigma2) / (sum(model_metafor_mv$sigma2) + 
                                                  (model_metafor_mv$k-model_metafor_mv$p)/sum(diag(P)))
I2_statistic


############## 5. Check outliers ############## 

# Using standardized residuals of correlations (and create png)
resid <- residuals(model_metafor_mv) %>%
  scale(center = FALSE, scale = TRUE)  # convert residuals to z-scores

par(mar=c(6,6,4,4))  # change margins to use full space
plot(resid, type="o", pch=19)
png(filename = "ResidualsPlot.png", 
    width = 800, height = 640, 
    pointsize = 12, res = 120)
plot(resid, type="o", pch=19)
dev.off()

outliers_resid <- resid %>%
  cbind(ef_decod_data$Study) %>%            # bind study names for reference
  subset(resid > 3.0 | resid < - 3.0) %>%   # subset outliers
  View()

# Using Cook's distance (and create png)
cooks <- cooks.distance(model_metafor_mv)
plot(cooks, type="o", pch=19)
png(filename = "CooksDistancePlot.png", 
    width = 800, height = 640, 
    pointsize = 12, res = 120)
plot(cooks, type="o", pch=19)
dev.off()

# View outliers with Cooks > 3 * mean
outliers_cooks <- cooks %>% 
  cbind(ef_decod_data$Study) %>%           # bind study names for reference
  subset(cooks > 3.0*mean(cooks)) %>%      # subset outliers
  View()

# Create new dataframe with outliers removed
ef_decod_data_no_outliers <- ef_decod_data %>%
  cbind(cooks) %>%
  filter(cooks < 3.0*mean(cooks))



############## 6. Create forest plot ############## 

# Save output as pdf
pdf("ForestPlot.pdf", family = "Courier", width = 10, height = 8.5)

# Decrease margins so the full space is used
par(mar=c(2.5,4,1,2.5), cex = .9, font = 1)

# Create the forest plot
forest(model_metafor_mv, 
       xlim = c(-2.5, 1.8),                 # adjust horizontal plot region limits
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
       mlab = "RE Model: p = .04, I2 = 93.9",    # label summary estimate
       showweights=F,                       # include weights given to effects in model fitting
       steps = 5)                           # number of tick marks for the x-axis

# Switch to bold font
par(cex = .9, font = 2)

# Add column headings to the plot
text(-2.5, 22, "Study name", pos = 4, cex = .9)
text(1.8, 22, "Correlation and 95% CI", pos = 2, cex = .9)

# Close off set par back to the original settings
dev.off()

op <- par(cex = .9, font = 1)
par(op)


############## 7. Run moderator analyses ############## 

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


############## 8. Test for biases ############## 

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
#pdf("FunnelPlot.pdf", width=7, height=5)
par(mar=c(4.5,4.5,1,1))
taf <- trimfill(model_trim_fill) # estimate missing studies
funnel(taf,                      # add missing studies to plot
       xlim=c(-1,1), 
       xlab= "Correlation", 
       ylim = c(.24, 0), 
       steps = 4, 
       digits = c(1, 2))
par(mar=c(2.5,3.6,0,1.5))
# dev.off()

# Using p-curve
# The content of the .csv file generated below can be copy-pasted
# into www.p-curve.com/app4/ to create the p-curve image

degrees_freedom <- ef_decod_data_no_outliers$NSubjects - 2
p_curve_data <- paste("R(", degrees_freedom, ")=", ef_decod_data_no_outliers$yi, sep="")
print(p_curve_data, row.names=FALSE)
# write.table(p_curve_data, "p-CurveData.csv", sep=",", row.names=FALSE, col.names=FALSE)


