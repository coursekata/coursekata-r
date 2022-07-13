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
#' department. **PloS One, 14**(1), e0209232. <https://doi.org/10.1371/journal.pone.0209232>
#'
#' @format A data frame with 84 observations on the following 53 variables:
#' \describe{
#'   \item{\code{id}}{Subject ID}
#'   \item{\code{condition}}{Whether the subject saw a **Dog** or was in the **Control** group}
#'   \item{\code{age}}{Subject's age in years}
#'   \item{\code{gender}}{Subject's self-identified gender}
#'   \item{\code{race}}{Subject's self-identified race}
#'   \item{\code{veteran}}{Is the subject a veteran?}
#'   \item{\code{disabled}}{Is the subject disabled?}
#'   \item{\code{dog_name}}{The name of the therapy dog}
#'   \item{\code{base_pain}}{Subject's self reported pain before the intervention (T0)}
#'   \item{\code{base_depression}}{Subject's self reported depression before the intervention (T0)}
#'   \item{\code{base_anxiety}}{Subject's self reported anxiety before the intervention (T0)}
#'   \item{\code{base_total}}{The sum of the subject's \code{base_*} scores}
#'   \item{\code{later_pain}}{Subject's self reported pain after the intervention (T1)}
#'   \item{\code{later_depression}}{Subject's self reported depression after the intervention (T1)}
#'   \item{\code{later_anxiety}}{Subject's self reported anxiety after the intervention (T1)}
#'   \item{\code{later_total}}{The sum of the subject's \code{later_*} scores}
#'   \item{\code{last_pain}}{Subject's self reported pain after the intervention (T2)}
#'   \item{\code{last_depression}}{Subject's self reported depression after the intervention (T2)}
#'   \item{\code{last_anxiety}}{Subject's self reported anxiety after the intervention (T2)}
#'   \item{\code{last_total}}{The sum of the subject's \code{last_*} scores}
#'   \item{\code{change_pain}}{The change in subject's pain from before the intervention to after}
#'   \item{\code{change_depression}}{The change in subject's depression from before the intervention
#'     to after}
#'   \item{\code{change_anxiety}}{The change in subject's anxiety from before the intervention to
#'     after}
#'   \item{\code{change_total}}{The sum of the subject's \code{change_*} scores}
#'   \item{\code{provider_male}}{Was the health care provider male?}
#'   \item{\code{provider}}{The health care provider's status: either an **Advanced Practicioner**,
#'     **Resident** physician, or **Attending** physician}
#'   \item{\code{heart_rate}}{The subject's heart rate at baseline (T0)}
#'   \item{\code{resp_rate}}{The subject's respiratory rate at baseline (T0)}
#'   \item{\code{sp_o2}}{The subject's SpO2 at baseline (T0)}
#'   \item{\code{bp_syst}}{The subject's systolic blood pressure at baseline (T0)}
#'   \item{\code{bp_diast}}{The subject's diastolic blood pressure at baseline (T0)}
#'   \item{\code{med_given}}{Was the subject given medication prior to the study? (T0)}
#'   \item{\code{mh_none}}{None of the other medical history items were indicated}
#'   \item{\code{mh_asthma}}{Medical history: asthma}
#'   \item{\code{mh_smoker}}{Medical history: smoker}
#'   \item{\code{mh_cad}}{Medical history: coronary artery disease}
#'   \item{\code{mh_diabetes}}{Medical history: diabetes mellitus}
#'   \item{\code{mh_hypertension}}{Medical history: hypertension}
#'   \item{\code{mh_stroke}}{Medical history: prior stroke}
#'   \item{\code{mh_chronic_kidney}}{Medical history: chronic kidney disease}
#'   \item{\code{mh_copd}}{Medical history: chronic obstructive pulmonary disease}
#'   \item{\code{mh_hyperlipidemia}}{Medical history: hyperlipidemia}
#'   \item{\code{mh_hiv}}{Medical history: HIV}
#'   \item{\code{mh_other}}{Medical history: other (write-in)}
#'   \item{\code{ph_adhd}}{Psychiatric history: attention-deficit/hyperactivity disorder}
#'   \item{\code{ph_anxiety}}{Psychiatric history: anxiety}
#'   \item{\code{ph_bipolar}}{Psychiatric history: bipolar}
#'   \item{\code{ph_borderline}}{Psychiatric history: borderline personality disorder}
#'   \item{\code{ph_depression}}{Psychiatric history: depression}
#'   \item{\code{ph_schizophrenia}}{Psychiatric history: schizophrenia}
#'   \item{\code{ph_ptsd}}{Psychiatric history: PTSD}
#'   \item{\code{ph_none}}{None of the other psychiatric history items were indicated}
#'   \item{\code{ph_other}}{Psychiatric history: other (write-in)}
#' }
"er"

#' Simulated housing data
#'
#' These data are simulated to be similar to the Ames housing data, but with far fewer variables
#' and much smaller effect sizes.
#'
#' @format A data frame with 32 observations on the following 4 variables:
#' \describe{
#'   \item{\code{PriceK}}{Price the home sold for (in thousands of dollars)}
#'   \item{\code{Neighborhood}}{The neighborhood the home is in (Eastside, Downtown)}
#'   \item{\code{HomeSizeK}}{The size of the home (in thousands of square feet)}
#'   \item{\code{HasFireplace}}{Whether the home has a fireplace (0 = no, 1 = yes)}
#' }
"Smallville"


#' Data from introductory statistics students at a university.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 157 observations on the following 16 variables:
#' \describe{
#'   \item{\code{Sex}}{Sex of participant.}
#'   \item{\code{RaceEthnic}}{Racial or ethnic background.}
#'   \item{\code{FamilyMembers}}{Members of immediate family (excluding self).}
#'   \item{\code{SSLast}}{Last digit of social security number (\code{NA} if no SSN).}
#'   \item{\code{Year}}{Year in school: \code{1}=First, \code{2}=Second, \code{3}=Third,
#'     \code{4}=Fourth, \code{5}=Other}
#'   \item{\code{Job}}{Current employment status: \code{1}=Not Working, \code{2}=Part-time Job,
#'     \code{3}=Full-time Job}
#'   \item{\code{MathAnxious}}{Agreement with the statement "In general I tend to feel very anxious
#'     about mathematics": \code{1}=Strongly Disagree, \code{2}=Disagree, \code{3}=Neither Agree nor
#'     Disagree, \code{4}=Agree, \code{5}=Strongly Agree}
#'   \item{\code{Interest}}{Interest in statistics and the course: \code{1}=No Interest,
#'     \code{2}=Somewhat Interested, \code{3}=Very Interested}
#'   \item{\code{GradePredict}}{Numeric prediction for final grade in the course. The value is
#'     converted from the student's letter grade prediction. \code{4.0}=A, \code{3.7}=A-,
#'     \code{3.3}=B+, \code{3.0}=B, \code{2.7}=B-, \code{2.3}=C+, \code{2.0}=C, \code{1.7}=C-,
#'     \code{1.3}=Below C-}
#'   \item{\code{Thumb}}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{\code{Index}}{Length in mm from tip of index finger to the crease between the index
#'     finger and palm.}
#'   \item{\code{Middle}}{Length in mm from tip of middle finger to the crease between the middle
#'     finger and palm.}
#'   \item{\code{Ring}}{Length in mm from tip of ring finger to the crease between the middle finger
#'     and palm.}
#'   \item{\code{Pinkie}}{Length in mm from tip of pinkie finger to the crease between the pinkie
#'     finger and palm.}
#'   \item{\code{Height}}{Height in inches.}
#'   \item{\code{Weight}}{Weight in pounds.}
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
#'   \item{\code{Sex}}{Sex of participant.}
#'   \item{\code{RaceEthnic}}{Racial or ethnic background.}
#'   \item{\code{FamilyMembers}}{Members of immediate family (excluding self).}
#'   \item{\code{SSLast}}{Last digit of social security number (\code{NA} if no SSN).}
#'   \item{\code{Year}}{Year in school: \code{1}=First, \code{2}=Second, \code{3}=Third,
#'     \code{4}=Fourth, \code{5}=Other}
#'   \item{\code{Job}}{Current employment status: \code{1}=Not Working, \code{2}=Part-time Job,
#'     \code{3}=Full-time Job}
#'   \item{\code{MathAnxious}}{Agreement with the statement "In general I tend to feel very anxious
#'     about mathematics": \code{1}=Strongly Disagree, \code{2}=Disagree, \code{3}=Neither Agree nor
#'     Disagree, \code{4}=Agree, \code{5}=Strongly Agree}
#'   \item{\code{Interest}}{Interest in statistics and the course: \code{1}=No Interest,
#'     \code{2}=Somewhat Interested, \code{3}=Very Interested}
#'   \item{\code{GradePredict}}{Numeric prediction for final grade in the course. The value is
#'     converted from the student's letter grade prediction. \code{4.0}=A, \code{3.7}=A-,
#'     \code{3.3}=B+, \code{3.0}=B, \code{2.7}=B-, \code{2.3}=C+, \code{2.0}=C, \code{1.7}=C-,
#'     \code{1.3}=Below C-}
#'   \item{\code{Thumb}}{Length in mm from tip of thumb to the crease between the thumb and palm.}
#'   \item{\code{Index}}{Length in mm from tip of index finger to the crease between the index
#'     finger and palm.}
#'   \item{\code{Middle}}{Length in mm from tip of middle finger to the crease between the middle
#'     finger and palm.}
#'   \item{\code{Ring}}{Length in mm from tip of ring finger to the crease between the middle finger
#'     and palm.}
#'   \item{\code{Pinkie}}{Length in mm from tip of pinkie finger to the crease between the pinkie
#'     finger and palm.}
#'   \item{\code{Height}}{Height in inches.}
#'   \item{\code{Weight}}{Weight in pounds.}
#' }
"Fingers.messy"


#' Generated "class data" for exploring pairwise tests
#'
#' These data were generated as outcomes for "students" for three different "instructors" named
#' A, B, and C. The outcome have means such that C > B > A, but the difference is only clearly
#' significant for C > A, and borderline for the others.
#'
#' \describe{
#'   \item{\code{outcome}}{A hypothetical, numerical outcome of an intervention.}
#'   \item{\code{teacher}}{Either "A", "B", or "C", associating the outcome to a teacher.}
#' }
"class_data"


#' Tables data
#'
#' Data about tips collected from an experiment with 44 tables at a restaurant.
#'
#' @format A data frame with 44 observations on the following 2 variables.
#' \describe{
#'   \item{\code{TableID}}{A number assigned to each table.}
#'   \item{\code{Tip}}{How much the tip was.}
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
#'   \item{\code{TableID}}{A number assigned to each table.}
#'   \item{\code{Tip}}{How much the tip was.}
#'   \item{\code{Condition}}{Which experimental condition the table was randomly assigned to.}
#'   \item{\code{Check}}{The amount of money the table paid for their meal.}
#' }
"TipExperiment"


#' Students at a university were asked to enter a random number between 1-20 into a survey.
#'
#' Students at a university taking an introductory statistics course were asked to complete this
#' survey as part of their homework.
#'
#' @format A data frame with 211 observations on the following 1 variable:
#' \describe{
#'   \item{\code{Any1_20}}{The random number between 1 and 20 that a student thought of.}
#' }
"Survey"
