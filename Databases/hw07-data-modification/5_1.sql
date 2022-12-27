create view StudentMarks as
select StudentId, count(Mark) as Marks
from
  Students
  left join Marks
    using (StudentId)
group by StudentId
