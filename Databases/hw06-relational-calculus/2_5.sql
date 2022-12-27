select distinct
  StudentId, StudentName, GroupName
from
  Students, Groups, Plan, Courses c1
where
  Students.GroupId = Groups.GroupId
  and Groups.GroupId = Plan.GroupId
  and Plan.CourseId = c1.CourseId
  and c1.CourseName = :CourseName
  and StudentId not in (
      select
        StudentId
      from
        Marks, Courses c2
      where
        Marks.CourseId = c2.CourseId
        and CourseName = :CourseName
    )
