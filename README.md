## P8131 Spring 2022 Homework #5

1.	In a study of nesting horseshoe crabs, each female horseshoe crab had a male crab attached to her in her nest. The study investigated factors that affect whether the female crab had any other males, called satellites, residing near her. Explanatory variables that are thought to affect this included the female crab’s color (C), spine condition (S), carapace width (W) and weight (Wt). The response outcome for each female crab is her number of satellites (Sa). There are 173 females in this study. Data are provided in the crab.txt.  
(a)	Fit a Poisson model (M1) with log link with W as the single predictor. Check the goodness of fit and interpret your model.  
(b)	Fit a model (M2) with W and Wt as predictors. Compare it with the model in (a). Interpret your results.  
(c)	Check overdispersion in M2. Interpret the model after adjusting for overdispersion.  

2.	Researchers examined a large number of fish to determine the prevalence of parasites. The dataset (parasite.txt) includes the variables Intensity (i.e., the number of parasites), Area (a categorical variable), Year (to be treated as categorical), and Length of the fish.  
(a)	Fit a Poisson model with log link to the data with area, year, and length as predictors. Interpret each model parameter.  
(b)	Test for the goodness of fit of the model in (a) and state conclusions.  
(c)	Researchers suspect that there may be two strains of fish, one that is susceptible to parasites and one that is not. Without knowing which fish are susceptible, this could be regarded as a zero-inflated model. Building on the model in (a) (using the same predictors), fit an appropriate model to the data that can account for extra zeros. Provide an interpretation for each model parameter in terms of the problem.  
