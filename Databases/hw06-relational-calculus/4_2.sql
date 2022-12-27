select
  StudentName, CourseName
from
  Students s1,
  Courses,
  (
    select distinct s2.StudentId, Plan.CourseId
    from
      Students s2,
      Plan,
      Marks
    where
      s2.GroupId = Plan.GroupId
      and s2.StudentId = Marks.StudentId
      and Plan.CourseId = Marks.CourseId
      and Mark <= 2
  ) SCQuery
where
  s1.StudentId = SCQuery.StudentId
  and Courses.CourseId = SCQuery.CourseId
