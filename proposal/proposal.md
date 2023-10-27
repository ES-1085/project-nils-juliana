Tidy Courses Proposal
================
Nils and Juliana

``` r
library(tidyverse)
library(broom)
```

``` r
coa_courses <- read_excel("data/coa-courses.xlsx")
coa_courses2 <- read_csv("data/coa_courses2.csv")
```

    ## Rows: 1327 Columns: 15
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (13): Department, CourseNumber, CourseDivision, CourseName, Instructor, ...
    ## dbl  (1): Year
    ## lgl  (1): ActiveFlag
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
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
         Lab_Fee_USD = str_extract(CourseDescription, pattern = "(?<=Lab fee\\: \\$)[^.]+"),
         Class_Size = str_extract(CourseDescription, pattern = "(?<=Class limit\\: )[^.]+"), # Try adapting for course fee
         Degree_Requirements = str_extract(CourseDescription, pattern = "(?<=Meets the following degree requirements\\: ).*"))

coa_courses3 <- coa_courses %>%
   separate_rows(Degree_Requirements, sep = ", ") %>%
   separate_rows(Degree_Requirements, sep = " ") %>%
   separate_rows(Degree_Requirements, sep = ",")

coa_courses3 %>%
  # distinct(Department, CourseNumber, Term, Year) %>% # This will be added to drop duplicates
  filter(Degree_Requirements %in% c("ES", "ADS", "AD", "QR", "HS", "HY")) %>%

  group_by(Year, Degree_Requirements) %>%
  summarize(number_courses_per_degree = n()) %>%
  ggplot(aes(x = Year, y = number_courses_per_degree, color = Degree_Requirements)) +
  geom_line(aes(group = Degree_Requirements))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the
    ## `.groups` argument.

![](proposal_files/figure-gfm/import-data-add-variables-1.png)<!-- -->

``` r
write_csv(coa_courses, file = "data/coa_courses2.csv")
```

``` r
instructors <- coa_courses2 %>% distinct(Instructor, Year)

instructors_distinct <- coa_courses2 %>% distinct(Instructor)

write_csv(instructors_distinct, file = "data/instructors.csv")

instructors <- instructors %>%
  mutate(Status = case_when(Instructor %in% c("Anderson, John", 
                                              "Andrews, Nancy",
                                              "Baker, Jodi",
                                              "Baker, Laurie",
                                              "Borden, Richard",
                                              "Carpenter, William",
                                              "Cline, Ken",
                                              "Clinger, Catherine",
                                              "Colbert, Dru",
                                              "Collum, Kourtney",
                                              "Cooper, John",
                                              "Cox, Gray",
                                              "Feldman, David",
                                              "Foley, Sean",
                                              "Friedlander, Jay",
                                              "Gatti, Daniel",
                                              "Hall, Sarah",
                                              "Henderson, Jonathan",
                                              "Hess, Helen",
                                              "Hill, Kenneth",
                                              "Hudson, Reuben",
                                              "Kozak, Anne",
                                              "Letcher, Susan",
                                              "Little-Siebold, Todd",
                                              "Mancinelli, Isabel",
                                              "McKown, Jamie",
                                              "Morse, Suzanne",
                                               "Petersen Christopher",
                                              "Ressel, Stephen",
                                              "Schrade, Daniel Kojo",
                                              "Stabinsky, Doreen",
                                              "Tai, Bonnie",
                                              "Taneja , Palak",
                                              "Tardif, Twila",
                                              "Taylor, Davis",
                                              "Todd, Sean",
                                               "van Vliet, Netta",
                                                "Visvader, John",
                                                "Waldron, Karen") ~ "Permanent Faculty",
                          Instructor %in% c("Pena, Karla") & Year <= 2022 ~ "Lecturer",
                          Instructor %in% c("Pena, Karla") & Year > 2022 ~ "Permanent Faculty",
                           Instructor %in% c("Lakey, Heather") & Year <= 2021 ~ "Adjunct",
                          Instructor %in% c("Lakey, Heather") & Year > 2021 ~ "Permanent Faculty",
                            Instructor %in% c("Soares, Zachary") & Year >= 2022 ~ "Teaching Staff",
                            Instructor %in% c("Soares, Zachary") & Year < 2022 ~ "Adjunct",
                            Instructor %in% c("Cass, Blake", 
                                              "Fuller, Linda",
                                              "Gibson, David",
                                              "Teaching Staff",
                                              "Longsworth, Gordon") ~ "Teaching Staff",
                            Instructor %in% c("Beach, Desmond",
                                              "Chien, Ming-Tso",
                                              "Downing, E. Saffronia",
                                              "Oblongata, Donna") ~ "Visitor",
                           Instructor %in% c("Capers, Colin",
                                             "Donovan, Martha",
                                             "Levin, Robert",
                                             "Mahoney, Daniel",
                                              "Stover, Candice",
                                              "Swann, Scott",
                                              "Turok, Katharine",
                                              "Weber, Jill",
                                               "Winer, Joshua") ~ "Lecturer", TRUE ~ "Adjunct"))


coa_courses2 <- coa_courses2 %>%
  left_join(instructors, by = c("Instructor", "Year"))

# An example of exploring a hypothetical what could count for resource area requirements
#coa_courses2 <- coa_courses2 %>%
  #mutate(course_counts = case_when(Status %in% c("Permanent Teaching Staff", "Permanent full-time", "Permanent part-time")) ~ "Count",
        # TRUE ~ "Doesn't Count")
```

## 1. Introduction

Every term during the registration period, the COA community navigates
the system of academic courses categorized by resource area
(department), course level, and the fulfillment of graduation
requirements. These categories, which shape decisions on the system
offering every term, are not as logical or functional as they could be.
This situation has sparked a variety of interrelated research questions
for this project. Our team will focus on the following questions:

- How are COA course offerings distributed across resource areas?
- How well are COA course offerings meeting students’ graduation
  requirement needs?
- How many courses are professors and instructors teaching, and do those
  courses meet degree requirements?

If we receive more data from Lothar and time permits it, we would like
to explore the following questions as well:

- How many professors are there? Are they full-time faculty or else?
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
respective resource areas throughout the years/ terms. We will also
create visualizations to show how many classes meet degree requirements
per resource area. Additionally we will make note of how many professors
are teaching classes that meet degree requirements per term.

We might possibly extend or reorganize the current dataset in groupings
that include the status of each instructor, visiting faculty, faculty,
lecturer, teaching staff and so on. This information will be provided by
Krystal, the Registrar, and Lothar, Academic Services Administrator.
