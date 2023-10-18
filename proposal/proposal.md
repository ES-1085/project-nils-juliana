Tidy Courses Proposal
================
Nils and Juliana

``` r
library(tidyverse)
library(broom)
```

``` r
coa_courses <- read_excel("data/coa-courses.xlsx")

view(coa_courses)
```

``` r
glimpse(coa_courses)
```

    ## Rows: 1,327
    ## Columns: 8
    ## $ Department        <chr> "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AD"…
    ## $ CourseNumber      <chr> "1011", "1011", "1011", "1012", "1012", "1012", "101…
    ## $ CourseDivision    <chr> "ADS", "ADS", "ADS", "ADS", "ADS", "ADS", "AD HY", "…
    ## $ CourseName        <chr> "Introduction to Arts and Design", "Introduction to …
    ## $ Instructor        <chr> "Mancinelli, Isabel", "Mancinelli, Isabel", "Mancine…
    ## $ CourseDescription <chr> "This course is the fundamental course for students …
    ## $ ActiveFlag        <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
    ## $ Semester          <chr> "B18Q", "B19Q", "B20Q", "B20C", "B19B", "B18C", "B18…

``` r
coa_courses <- coa_courses %>%
  mutate(Year = str_c(20, str_extract(Semester, "\\d{2}")),
         Term = str_sub(Semester, start = 4, end = 4),
         Level = str_extract(CourseDescription, pattern = "Level: Intermediate|Level: Introductory|Level: Advanced"),
         Prerequisites = str_extract(CourseDescription, pattern = "Prerequisites[^.]+"),
         Class_Size = str_extract(CourseDescription, pattern = "Class limit[^.]+"),
         Class_Size = str_extract(CourseDescription, pattern = "Class limit[^.]+"), # Try adapting for course fee
         Degree_Requirements = str_extract(CourseDescription, pattern = "Meets the following degree requirements[^.]+"))
```

## 1. Introduction

Every term during the registration period, the COA community navigates
the system of academic courses categorized by resource area
(department), course level, and the fulfillment of graduation
requirements. These categories, which shape decisions on the system
offering every term, are not as logical or functional as they could be.
This situation has sparked a variety of interrelated research questions
for this project as follows:

- How are COA course offerings distributed across resource areas?
- How well are COA course offerings meeting students’ graduation
  requirement needs?
- How many professors are there? Are they full-time faculty or else?
- How many courses are professors and instructors teaching, and do those
  courses meet degree requirements?
- How are professors and other kinds of instructors distributed among
  resource areas?

The dataset we will use for this project was collected from the
Registrar records from the Winter of 2018 to the Spring of 2023. This
dataset contains 1327 observations or courses offered in an academic
year. Each observation has 14 variables: Department, Course Number,
Course Division, Course Name, Instructor, Course Description, Active
Flag, Semester, Year, Term, Level, Prerequisites, Class Size, and Degree
Requirements

## 2. Data

- \[Yes\] Is the data in the /data folder?
- \[Yes\] Does the README include the dimensions and codebook for the
  data set?
- \[Yes\] Does the proposal include the output of glimpse() or skim() of
  the data frame.

## 3. Ethics review

N/A

## 4. Data analysis plan

We will create a visualization showing course distribution in their
respective resource areas. Additionally, have this show the class sizes
and amount of students.

- Visualize classes within each resource areas that meet graduation
  requirement and portion that do not.
- Visualize a portion of classes taught by the different teacher
  groupings. Relate this to the amount of student spaces/enrollments.
- We will visualize classes that meet the graduation requirements.

We might possibly extend or reorganize the current dataset in groupings
that include the status of each instructor, visiting faculty, faculty,
lecturer, teaching staff and so on. This information will be provided by
Krystal, the Registrar, and Lothar, Academic Services Administrator.
