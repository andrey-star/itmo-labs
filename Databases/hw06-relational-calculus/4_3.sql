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
      and not exists (
        select
          StudentId, CourseId
        from
          Marks
        where
          Marks.CourseId = Plan.CourseId
          and Marks.StudentId = s2.StudentId
          and Mark > 2
      )
  ) SCQuery
where
  s1.StudentId = SCQuery.StudentId
  and Courses.CourseId = SCQuery.CourseId
