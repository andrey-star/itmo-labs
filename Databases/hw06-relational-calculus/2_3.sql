select distinct
  StudentId, StudentName, GroupName
from
  Students, Groups
where
  Students.GroupId = Groups.GroupId
  and StudentId not in (
      select
        StudentId
      from
        Marks, Courses
      where
        Marks.CourseId = Courses.CourseId
        and CourseName = :CourseName
    )
