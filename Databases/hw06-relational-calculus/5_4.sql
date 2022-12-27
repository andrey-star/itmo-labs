select StudentId
from Students s1
where StudentId not in (
  select s2.StudentId
  from
    Students s2,
    Lecturers,
    Plan
  where
    s2.GroupId = Plan.GroupId
    and Plan.LecturerId = Lecturers.LecturerId
    and Lecturers.LecturerName = :LecturerName
    and not exists (
      select
        StudentId, CourseId
      from
        Marks
      where
        Marks.CourseId = Plan.CourseId
        and s2.StudentId = Marks.StudentId
    )
)
