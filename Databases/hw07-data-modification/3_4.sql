update Students
set marks = (
  select count(distinct CourseId)
  from Marks
  where Marks.StudentId = Students.StudentId
)
