create view Debts as
select StudentId, count(distinct CourseId) as Debts
from
  Students
  natural join Plan
  left join Marks
    using (StudentId, CourseId)
where Mark is null
group by StudentId
