#' Ames, Iowa housing data
#'
#' @description
#' Data describing all residential home sales in Ames, Iowa from the years 2006–2010 as reported by
#' the Ames City Assessor's Office and compiled by De Cock (2011). Ames is located about 30 miles
#' north of Des Moines (the stats capitol) and is home to Iowa State University (the largest
#' university in the state). Each row represents the latest sale of a home (one row per home in the
#' dataset). Columns represent home features and sale prices (outcome). The original dataset
#' includes a uniquely detailed (81 features per home) and comprehensive look at the housing market.
#' The data included here are only a subset used for examples in CourseKata course material. See
#' the references and data source for the full dataset.
#'
#' ### Pedagogical Modifications
#' To simplify the dataset for instructional purposes, the data were filtered to include only single
#' family homes, residential zoning, 1-2 story homes, homes with brick, cinder block, or concrete
#' foundations, and average to excellent kitchen qualities. Further, the descriptive variables were
#' reduced to the subset described in the format section.
#'
#' @source <https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data>
#'
#' @references
#' De Cock, Dean, (2011). Ames, Iowa: Alternative to the Boston Housing Data as an end of semester
#' regression project, *Journal of Statistics Education, 19*(3).
#' \doi{10.1080/10691898.2011.11889627}
#'
#' @format A data frame with 2930 observations on the following 80 variables:
#' \describe{
#'   \item{`YearBuilt`}{Year home was built (`YYYY`).}
#'   \item{`YearSold`}{Year of home sale (`YYYY`). Note: all home sales in this dataset occurred
#'     between 2006 - 2010. If a home was sold more than once between 2006 - 2010, only its latest
#'     sale is included in dataset.}
#'   \item{`Neighborhood`}{One of two neighborhoods in Ames county: \itemize{
#'     \item{College Creek (`CollegeCreek`), a neighborhood located adjacent to Iowa State
#'       University (the largest University in the state).}
#'     \item{Old Town (`OldTown`), a nationally designated historic district in Ames. The old
#'       neighborhood is located just north of the central business district.}
#'   }}
#'   \item{`HomeSizeR`}{Raw above-ground area of home, measured in square feet.}
#'   \item{`HomeSizeK`}{Above-ground area of home, measured in thousands of square feet.}
#'   \item{`LotSizeR`}{Raw total property lot size, measured in square feet.}
#'   \item{`LotSizeK`}{Total property lot size, in thousands of square feet.}
#'   \item{`Floors`}{Number of above-ground floors (1 story or 2 story).}
#'   \item{`BuildQuality`}{Assessor's rating of overall material and finish of the house. \itemize{
#'     \item{`10`: Very Excellent}
#' 	   \item{`9`: Excellent}
#'     \item{`8`: Very Good}
#'     \item{`7`: Good}
#'     \item{`6`: Above Average}
#'     \item{`5`: Average}
#'     \item{`4`: Below Average}
#'     \item{`3`: Fair}
#'     \item{`2`: Poor}
#'     \item{`1`: Very Poor}
#'   }}
#'   \item{`Foundation`}{Type of foundation (ground material underneath the house). \itemize{
#'     \item{`Brick&Tile`: Brick and Tile}
#'     \item{`CinderBlock`: Cinder Blocks}
#'     \item{`PouredConcrete`: Poured Concrete}
#'   }}
#'   \item{`HasCentralAir`}{Indicator if home contains central air conditioning (0 = No, 1 = Yes).}
#'   \item{`Bathrooms`}{Number of full above-ground bathrooms.}
#'   \item{`Bedrooms`}{Number of full above-ground bedrooms.}
#'   \item{`TotalRooms`}{Number of above-ground rooms in home, excluding bathrooms.}
#'   \item{`KitchenQuality`}{Assessor's rating of kitchen material quality. \itemize{
#'     \item{`Excellent`}
#'     \item{`Good`}
#'     \item{`Average`}
#'   }}
#'   \item{`HasFireplace`}{Indicator if home contains at least one fireplace (0 = No, 1 = Yes).}
#'   \item{`GarageType`}{Type of garage. \itemize{
#'     \item{`Attached`: includes attached, built-in, basement, and dual-type garages}
#'     \item{`Detached`: includes detached and carport garages}
#'     \item{`None`: home does not have a garage or carport}
#'   }}
#'   \item{`GarageCars`}{Number of cars that can fit in garage.}
#'   \item{`PriceR`}{Sale price of home, in raw USD ($)}
#'   \item{`PriceK`}{Sale price of home, in thousands of USD ($)}
#'   \item{`TinySet`}{(Ignore) Whether or not this row is in `ames_tiny.csv`}
#' }
"Ames"


#' Emergency room canine therapy
#'
#' @description
#' Data from: Controlled clinical trial of canine therapy versus usual care to reduce patient
#' anxiety in the emergency department.
#'
#' ## Abstract
#'
#' ### Objective
#' Test if therapy dogs can reduce anxiety in emergency department (ED) patients.
#'
#' ### Methods
#' In this controlled clinical trial (NCT03471429), medically stable, adult patients
#' were approached if the physician believed that the patient had “moderate or greater anxiety.”
#' Patients were allocated on a 1:1 ratio to either 15 min exposure to a certified therapy dog and
#' handler (dog), or usual care (control). Patient reported anxiety, pain and depression were
#' assessed using a 0-10 scale (10=worst). Primary outcome was change in anxiety from baseline (T0)
#' to 30 min and 90 min after exposure to dog or control (T1 and T2 respectively); secondary
#' outcomes were pain, depression and frequency of pain medication.
#'
#' ### Results
#' Among 98 patients willing to participate in research, 7 had aversions to dogs, leaving 91 (93%)
#' were willing to see a dog; 40 patients were allocated to each group (dog or control). No data
#' were normally distributed. Median baseline anxiety, pain and depression were similar between
#' groups. With dog exposure, anxiety decreased significantly from T0 to T1: 6 (IQR 4-9.75) to T1:
#' 2 (0-6) compared with 6 (4-8) to 6 (2.5-8) in controls (P<0.001, for T1, Mann-Whitney U). Dog
#' exposure was associated with significantly lower anxiety at T2 and a significant overall
#' treatment effect on two-way repeated measures ANOVA for anxiety, pain and depression. After
#' exposure, 1/40 in the dog group needed pain medication, versus 7/40 in controls (P=0.056,
#' Fisher’s).
#'
#' ### Conclusions
#' Exposure to therapy dogs plus handlers significantly reduced anxiety in ED patients.
#'
#' @references
#' Kline, J. A., Fisher, M. A., Pettit, K. L., Linville, C. T., & Beck, A. M. (2019). Controlled
#' clinical trial of canine therapy versus usual care to reduce patient anxiety in the emergency
#' department. *PloS One, 14*(1), e0209232. \doi{10.1371/journal.pone.0209232}
#'
#' @format A data frame with 84 observations on the following 53 variables:
#' \describe{
#'   \item{`id`}{Subject ID}
#'   \item{`condition`}{Whether the subject saw a `Dog` or was in the `Control` group}
#'   \item{`age`}{Subject's age in years}
#'   \item{`gender`}{Subject's self-identified gender}
#'   \item{`race`}{Subject's self-identified race}
#'   \item{`veteran`}{Is the subject a veteran?}
#'   \item{`disabled`}{Is the subject disabled?}
#'   \item{`dog_name`}{The name of the therapy dog}
#'   \item{`base_pain`}{Subject's self reported pain before the intervention (T0)}
#'   \item{`base_depression`}{Subject's self reported depression before the intervention (T0)}
#'   \item{`base_anxiety`}{Subject's self reported anxiety before the intervention (T0)}
#'   \item{`base_total`}{The sum of the subject's `base_*` scores}
#'   \item{`later_pain`}{Subject's self reported pain after the intervention (T1)}
#'   \item{`later_depression`}{Subject's self reported depression after the intervention (T1)}
#'   \item{`later_anxiety`}{Subject's self reported anxiety after the intervention (T1)}
#'   \item{`later_total`}{The sum of the subject's `later_*` scores}
#'   \item{`last_pain`}{Subject's self reported pain after the intervention (T2)}
#'   \item{`last_depression`}{Subject's self reported depression after the intervention (T2)}
#'   \item{`last_anxiety`}{Subject's self reported anxiety after the intervention (T2)}
#'   \item{`last_total`}{The sum of the subject's `last_*` scores}
#'   \item{`change_pain`}{The change in subject's pain from before the intervention to after}
#'   \item{`change_depression`}{The change in subject's depression from before the intervention
#'     to after}
#'   \item{`change_anxiety`}{The change in subject's anxiety from before the intervention to after}
#'   \item{`change_total`}{The sum of the subject's `change_*` scores}
#'   \item{`provider_male`}{Was the health care provider male?}
#'   \item{`provider`}{The health care provider's status: either an `Advanced Practitioner`,
#'     `Resident` physician, or `Attending` physician}
#'   \item{`heart_rate`}{The subject's heart rate at baseline (T0)}
#'   \item{`resp_rate`}{The subject's respiratory rate at baseline (T0)}
#'   \item{`sp_o2`}{The subject's SpO2 at baseline (T0)}
#'   \item{`bp_syst`}{The subject's systolic blood pressure at baseline (T0)}
#'   \item{`bp_diast`}{The subject's diastolic blood pressure at baseline (T0)}
#'   \item{`med_given`}{Was the subject given medication prior to the study? (T0)}
#'   \item{`mh_none`}{None of the other medical history items were indicated}
#'   \item{`mh_asthma`}{Medical history: asthma}
#'   \item{`mh_smoker`}{Medical history: smoker}
#'   \item{`mh_cad`}{Medical history: coronary artery disease}
#'   \item{`mh_diabetes`}{Medical history: diabetes mellitus}
#'   \item{`mh_hypertension`}{Medical history: hypertension}
#'   \item{`mh_stroke`}{Medical history: prior stroke}
#'   \item{`mh_chronic_kidney`}{Medical history: chronic kidney disease}
#'   \item{`mh_copd`}{Medical history: chronic obstructive pulmonary disease}
#'   \item{`mh_hyperlipidemia`}{Medical history: hyperlipidemia}
#'   \item{`mh_hiv`}{Medical history: HIV}
#'   \item{`mh_other`}{Medical history: other (write-in)}
#'   \item{`ph_adhd`}{Psychiatric history: attention-deficit/hyperactivity disorder}
#'   \item{`ph_anxiety`}{Psychiatric history: anxiety}
#'   \item{`ph_bipolar`}{Psychiatric history: bipolar}
#'   \item{`ph_borderline`}{Psychiatric history: borderline personality disorder}
#'   \item{`ph_depression`}{Psychiatric history: depression}
#'   \item{`ph_schizophrenia`}{Psychiatric history: schizophrenia}
#'   \item{`ph_ptsd`}{Psychiatric history: PTSD}
#'   \item{`ph_none`}{None of the other psychiatric history items were indicated}
#'   \item{`ph_other`}{Psychiatric history: other (write-in)}
#' }
"er"


#' Forced Expiratory Volume (FEV) Data
#'
#' @description
#' Data from: Fundamentals of Biostatistics
#' Notes from: Kahn, M.
#'
#' ## Abstract
#' Sample of 654 youths, aged 3 to 19, in the area of East Boston during middle to late 1970's.
#' Interest concerns the relationship between smoking and FEV. Since the study is necessarily
#' observational, statistical adjustment via regression models clarifies the relationship.
#'
#' ## Pedagogical Notes:
#' This is a versatile dataset that can be used throughout an introductory statistics course as
#' well as an introductory modeling course. It includes many issues from statistical adjustment
#' in observational studies, to subgroup analysis, quadratic regression and analysis of covariance.
#'
#' @references
#' Kahn,M. (2003). Data Sleuth, STATS, 37, 24. <https://jse.amstat.org/datasets/fev.txt>
#' Rosner, B. (1999). Fundamentals of Biostatistics, Pacific Grove, CA: Duxbury
#'
#' @format A data frame with 654 observations on the following 5 variables:
#' \describe{
#'   \item{`AGE`}{Age, in years}
#'   \item{`FEV`}{Forced expiratory volume, in liters}
#'   \item{`HEIGHT`}{Height, in inches}
#'   \item{`SEX`}{`0` = Female, `1` = Male}
#'   \item{`SMOKE`}{`0` = Non-smoker, `1` = Smoker}
#' }
"fevdata"


#' Data from introductory statistics students at a university.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 157 observations on the following 16 variables:
#' \describe{
#'   \item{`Gender`}{Gender of participant.}
#'   \item{`RaceEthnic`}{Racial or ethnic background.}
#'   \item{`FamilyMembers`}{Members of immediate family (excluding self).}
#'   \item{`SSLast`}{Last digit of social security number (`NA` if no SSN).}
#'   \item{`Year`}{Year in school: `1`=First, `2`=Second, `3`=Third, `4`=Fourth, `5`=Other}
#'   \item{`Job`}{Current employment status: `1`=Not Working, `2`=Part-time Job, `3`=Full-time Job}
#'   \item{`MathAnxious`}{Agreement with the statement "In general I tend to feel very anxious
#'     about mathematics": `1`=Strongly Disagree, `2`=Disagree, `3`=Neither Agree nor Disagree,
#'    `4`=Agree, `5`=Strongly Agree}
#'   \item{`Interest`}{Interest in statistics and the course: `1`=No Interest, `2`=Somewhat
#'     Interested, `3`=Very Interested}
#'   \item{`GradePredict`}{Numeric prediction for final grade in the course. The value is
#'     converted from the student's letter grade prediction. `4.0`=A, `3.7`=A-, `3.3`=B+, `3.0`=B,
#'     `2.7`=B-, `2.3`=C+, `2.0`=C, `1.7`=C-, `1.3`=Below C-}
#'   \item{`Thumb`}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{`Index`}{Length in mm from tip of index finger to the crease between the index
#'     finger and palm.}
#'   \item{`Middle`}{Length in mm from tip of middle finger to the crease between the middle
#'     finger and palm.}
#'   \item{`Ring`}{Length in mm from tip of ring finger to the crease between the middle finger
#'     and palm.}
#'   \item{`Pinkie`}{Length in mm from tip of pinkie finger to the crease between the pinkie
#'     finger and palm.}
#'   \item{`Height`}{Height in inches.}
#'   \item{`Weight`}{Weight in pounds.}
#'   \item{`Sex`}{Sex of participant.}
#' }
"Fingers"


#' Raw data from introductory statistics students at a university.
#'
#' This is the Fingers dataset before it was cleaned. In the cleaning process, we
#' converted the values from numbers to appropriate types (where applicable), removed
#' outliers that suggested data was input incorrectly, and we removed incomplete cases.
#' The description for the dataset is: Students at a university taking an introductory
#' statistics course were asked to complete this survey as part of their homework. (This
#' is the same data set as the Fingers data)
#'
#' @format A data frame with 157 observations on the following 16 variables:
#' \describe{
#'   \item{`Gender`}{Gender of participant.}
#'   \item{`RaceEthnic`}{Racial or ethnic background.}
#'   \item{`FamilyMembers`}{Members of immediate family (excluding self).}
#'   \item{`SSLast`}{Last digit of social security number (`NA` if no SSN).}
#'   \item{`Year`}{Year in school: `1`=First, `2`=Second, `3`=Third, `4`=Fourth, `5`=Other}
#'   \item{`Job`}{Current employment status: `1`=Not Working, `2`=Part-time Job, `3`=Full-time Job}
#'   \item{`MathAnxious`}{Agreement with the statement "In general I tend to feel very anxious
#'     about mathematics": `1`=Strongly Disagree, `2`=Disagree, `3`=Neither Agree nor Disagree,
#'    `4`=Agree, `5`=Strongly Agree}
#'   \item{`Interest`}{Interest in statistics and the course: `1`=No Interest, `2`=Somewhat
#'     Interested, `3`=Very Interested}
#'   \item{`GradePredict`}{Numeric prediction for final grade in the course. The value is
#'     converted from the student's letter grade prediction. `4.0`=A, `3.7`=A-, `3.3`=B+, `3.0`=B,
#'     `2.7`=B-, `2.3`=C+, `2.0`=C, `1.7`=C-, `1.3`=Below C-}
#'   \item{`Thumb`}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{`Index`}{Length in mm from tip of index finger to the crease between the index
#'     finger and palm.}
#'   \item{`Middle`}{Length in mm from tip of middle finger to the crease between the middle
#'     finger and palm.}
#'   \item{`Ring`}{Length in mm from tip of ring finger to the crease between the middle finger
#'     and palm.}
#'   \item{`Pinkie`}{Length in mm from tip of pinkie finger to the crease between the pinkie
#'     finger and palm.}
#'   \item{`Height`}{Height in inches.}
#'   \item{`Weight`}{Weight in pounds.}
#'   \item{`Sex`}{Sex of participant.}
#' }
"FingersMessy"


#' Generated "class data" for exploring pairwise tests
#'
#' These data were generated as outcomes for "students" for three different "instructors" named
#' A, B, and C. The outcome have means such that C > B > A, but the difference is only clearly
#' significant for C > A, and borderline for the others.
#'
#' \describe{
#'   \item{`outcome`}{A hypothetical, numerical outcome of an intervention.}
#'   \item{`teacher`}{Either "A", "B", or "C", associating the outcome to a teacher.}
#' }
"class_data"


#' Tables data
#'
#' Data about tips collected from an experiment with 44 tables at a restaurant.
#'
#' @format A data frame with 44 observations on the following 2 variables.
#' \describe{
#'   \item{`TableID`}{A number assigned to each table.}
#'   \item{`Tip`}{How much the tip was.}
#' }
"Tables"


#' Data from an experiment about smiley faces and tips
#'
#' Tables were randomly assigned to receive checks that either included or did not include a drawing
#' of a smiley face. Data was collected from 44 tables in an effort to examine whether the added
#' smiley face would cause more generous tipping.
#'
#' @format A data frame with 44 observations on the following 3 variables.
#' \describe{
#'   \item{`TableID`}{A number assigned to each table.}
#'   \item{`Tip`}{How much the tip was.}
#'   \item{`Condition`}{Which experimental condition the table was randomly assigned to.}
#'   \item{`Check`}{(Simulated) The amount of money the table paid for their meal.}
#'   \item{`FoodQuality`}{(Simulated) The perceived quality of the food.}
#' }
"TipExperiment"


#' Simulated data for an experiment about smiley faces and tips
#'
#' These are simulated data that are similar to the `TipExperiment` data. Hypothetical tables
#' were randomly assigned to receive checks that either included or did not include a drawing
#' of a smiley face, either from a male or a female server.
#'
#' @format A data frame with 44 observations on the following 3 variables.
#' \describe{
#'   \item{`gender`}{Whether the server was `female` or `male`}
#'   \item{`condition`}{Whether the check had a `smiley face` or not (`control`)}
#'   \item{`tip_percent`}{The size of the tip as a percentage of the price of the meal}
#' }
"tip_exp"


#' Simulated housing data
#'
#' These data are simulated to be similar to the Ames housing data, but with far fewer variables
#' and much smaller effect sizes.
#'
#' @format A data frame with 32 observations on the following 4 variables:
#' \describe{
#'   \item{`PriceK`}{Price the home sold for (in thousands of dollars)}
#'   \item{`Neighborhood`}{The neighborhood the home is in (Eastside, Downtown)}
#'   \item{`HomeSizeK`}{The size of the home (in thousands of square feet)}
#'   \item{`HasFireplace`}{Whether the home has a fireplace (0 = no, 1 = yes)}
#' }
"Smallville"


#' Students at a university were asked to enter a random number between 1-20 into a survey.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 211 observations on the following 1 variable:
#' \describe{
#'   \item{`Any1_20`}{The random number between 1 and 20 that a student thought of.}
#' }
"Survey"


#' Simulated math game data.
#'
#' The simulated results of a small study comparing the effectiveness of three different computer-
#' based math games in a sample of 105 fifth-grade students. All three games focused on the same
#' topic and had identical learning goals, and none of the students had any prior knowledge of the
#' topic.
#'
#' @format A data frame with 105 observations on the following 2 variables:
#' \describe{
#'   \item{`game`}{The game the student was randomly assigned to, coded as "A", "B", or "C".}
#'   \item{`outcome`}{Each student's score on the outcome test.}
#' }
"game_data"

#' A modified form of the [`palmerpenguins::penguins`] data set.
#'
#' The modifications are to select only a subset of the variables, and convert some of the units.
#'
#' @format A data frame with 333 observations on the following 7 variables:
#' \describe{
#'  \item{`species`}{The species of penguin, coded as "Adelie", "Chinstrap", or "Gentoo".}
#'  \item{`gentoo`}{Whether the penguin is a Gentoo penguin (1) or not (0).}
#'  \item{`body_mass_kg`}{The mass of the penguin's body, in kilograms.}
#'  \item{`flipper_length_m`}{The length of the penguin's flipper, in m.}
#'  \item{`bill_length_cm`}{The length of the penguin's bill, in cm.}
#'  \item{`female`}{Whether the penguin is female (1) or not (0).}
#'  \item{`island`}{The island where the penguin was observed, coded as "Biscoe", "Dream", or "Torgersen".}
#' }
"penguins"

#' Data on countries from the Happy Planet Index project.
#'
#' These data have been updated with some historical height data (from [Our World in
#' Data](https://ourworldindata.org/human-height)), drinking data (collected by the World Health
#' Organization featured in
#' [fivethirtyeight](https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/)),
#' population and land characteristics, and vaccination data (from March 2023).
#'
#' @format A data frame with 130 observations on the following 14 variables:
#' \describe{
#'  \item{`Country`}{Name of country}
#'  \item{`Region`}{One of 5 UN defined regions: Africa, Americas, Asia, Europe, Oceania}
#'  \item{`Code`}{Three-letter country codes defined by the International Organization for Standardization ([ISO](https://www.iso.org/iso-3166-country-codes.html)) to represent countries in a way that avoids errors since a country’s name changes depending on the language being used.}
#'  \item{`LifeExpectancy`}{Average life expectancy (in years)}
#'  \item{`GirlsH1900`}{The average of 18-year-old girls heights in 1900 (in cm)}
#'  \item{`GirlsH1980`}{The average of 18-year-old girls heights in 1980 (in cm)}
#'  \item{`Happiness`}{Score on a 0-10 scale for average level of happiness (10 being happiest)}
#'  \item{`GDPperCapita`}{Gross Domestic Product (per capita)}
#'  \item{`FertRate`}{The average number of children that will be born to a woman over her lifetime}
#'  \item{`PeopleVacc`}{Total number of people vaccinated in the country}
#'  \item{`PeopleVacc_per100`}{Total number of people vaccinated in the country (in percent)}
#'  \item{`Population2010`}{Population (in millions) in 2010}
#'  \item{`Population2020`}{Population (in millions) in 2020}
#'  \item{`WineServ`}{Average wine consumption per capita for those age 15 and over per week (collected by WHO)}
#' }
"World"
