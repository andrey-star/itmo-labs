create view AllMarks as
select StudentId, count(M.Mark) as Marks
from
  Students
  left join (
      select StudentId, CourseId, Mark
      from Marks
      union all
      select StudentId, CourseId, Mark
      from NewMarks
    ) as M
    using (StudentId)
group by StudentId