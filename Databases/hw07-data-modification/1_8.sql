delete from Students
where StudentId not in (
    select StudentId
    from
      Students
      natural join Plan
      left join Marks
        using (StudentId, CourseId)
    where Mark is null
    group by StudentId
    having count(distinct CourseId) >= 3
  )
