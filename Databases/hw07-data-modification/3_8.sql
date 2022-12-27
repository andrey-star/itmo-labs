update Students
set
  debts = (
    select count(distinct CourseId)
    from
      Students s
      natural join Plan
      left join Marks
        using (StudentId, CourseId)
    where Mark is null
      and s.StudentId = Students.StudentId
  ),
  marks = (
    select count(*)
    from Marks
    where Marks.StudentId = Students.StudentId
  )
