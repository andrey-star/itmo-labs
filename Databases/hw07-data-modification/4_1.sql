insert into Marks (StudentId, CourseId, Mark)
select StudentId, CourseId, NewMarks.Mark
from
  NewMarks
  left join Marks
    using (StudentId, CourseId)
  where
    Marks.Mark is null
