update Students
set marks = marks + (
  select count(*)
  from NewMarks
  where NewMarks.StudentId = Students.StudentId
)
