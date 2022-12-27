select GroupId, CourseId
from Groups, Courses c1
where not exists (
  select GroupId, CourseId
  from
    Students,
    Courses c2
  where
    not exists (
      select
        StudentId, CourseId
      from
        Marks
      where
        Marks.CourseId = c2.CourseId
        and Students.StudentId = Marks.StudentId
    )
    and Groups.GroupId = Students.GroupId
    and c1.CourseId = c2.CourseId
)
