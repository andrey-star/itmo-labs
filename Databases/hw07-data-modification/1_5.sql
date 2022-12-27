delete from Students
where
  StudentId in (
    select StudentId
    from Marks
    group by StudentId
    having count(*) <= 3
  )
  or
  StudentId not in (
    select StudentId
    from Marks
  )
