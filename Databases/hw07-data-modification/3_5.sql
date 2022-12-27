update Students
set debts = (
  select count(distinct CourseId)
  from
    Students
    natural join Plan
    left join Marks
      using (StudentId, CourseId)
  where Mark is null
    and Students.StudentId = :StudentId
)
where StudentId = :StudentId
