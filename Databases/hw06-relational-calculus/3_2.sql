select
  StudentName, CourseName
from
  Students s1,
  Courses,
  (
    select distinct StudentId, CourseId
    from
      Students s2, Plan
    where
      s2.GroupId = Plan.GroupId
    union
    select distinct StudentId, CourseId
    from
      Marks
  ) SCQuery
where
  s1.StudentId = SCQuery.StudentId
  and Courses.CourseId = SCQuery.CourseId
