update Students
set marks = (
  select count(*)
  from Marks
  where Marks.StudentId = :StudentId
)
where StudentId = :StudentId
