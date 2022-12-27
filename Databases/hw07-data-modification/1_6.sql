delete from Students
where
  StudentId in (
    select StudentId
    from
      Students
      natural join Plan
      left join Marks
        using (StudentId, CourseId)
    where Mark is null
  )
