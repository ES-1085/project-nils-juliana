# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## coa_courses
This dataset contains 1327, 14 variables. 

Observations are courses offered in an academic year. Each observation has 15 variables: Department, Course Number, Course Division, Course Name, Instructor, Course Description, Active Flag, Semester, Year, Term, Level, Prerequisites, Class Size, and Degree Requirements

- `Department`: Indicates the department associated with the course, Art and Design(AD), Environmental Science(ES), Human Studies(HS)
- `Course Number`: Course number associated with course, each course has a unique value. Course Number indicates the course level, introductary - intermediate -advanced  
- `Course Division`: Degree requirement meet 
- `Course Name`: Name of courses offered
- `Instructor`: Name of instructor
- `Course Description`: Shows the instructors descriptions of the course.
- `Active Flag`: Shows the class offered and active (true) and if outdated as (false)
- `Semester`: Indicates the year and term that class was offered. The number is the year and the letters q, b, c, represents fall, winter, spring respectively.
- `Year`: Shows the year of the class.
- `Term`: Shows the specific term. 
- `Level`: Indicates level of class, introductory-advanced.
- `Prerequisites`: Prerequisite classes required to take this class.
- `Class size`: Size of the class. 
- `Degree Requirements`: The degree requirement the course forfills.
- `Lab fee`: Cost of the class for the student to take it. 
