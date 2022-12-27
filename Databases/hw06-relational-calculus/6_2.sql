select
  GroupName, CourseName
from
  Groups g1,
  Courses c1,
  (
    select GroupId, CourseId
    from Groups g2, Courses c2
    where not exists (
      select GroupId, CourseId
      from
        Students,
        Courses c3
      where
        not exists (
          select
            StudentId, CourseId
          from
            Marks
          where
            Marks.CourseId = c3.CourseId
            and Students.StudentId = Marks.StudentId
        )
        and g2.GroupId = Students.GroupId
        and c2.CourseId = c3.CourseId
    )
  ) GCQuery
where
  g1.GroupId = GCQuery.GroupId
  and c1.CourseId = GCQuery.CourseId
