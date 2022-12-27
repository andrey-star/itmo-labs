select distinct StudentId
from Students s1
where StudentId not in (
  select s2.StudentId
  from
    Students s2,
    Lecturers,
    Plan,
    Marks
  where
    s2.StudentId = Marks.StudentId
    and s2.GroupId = Plan.GroupId
    and Lecturers.LecturerId = Plan.LecturerId
    and Plan.CourseId = Marks.CourseId
    and LecturerName = :LecturerName
)
