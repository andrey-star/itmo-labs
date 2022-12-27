create view StudentDebts as
select
  StudentId, coalesce(NonZero.Debts, 0) as Debts
from
  Students
  left join (
    select StudentId, count(distinct CourseId) as Debts
    from
      Students
      natural join Plan
      left join Marks
        using (StudentId, CourseId)
    where Mark is null
    group by StudentId
  ) NonZero
    using (StudentId)
