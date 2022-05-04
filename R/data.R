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
