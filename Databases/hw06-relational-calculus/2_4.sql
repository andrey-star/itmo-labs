select distinct
  StudentId, StudentName, GroupName
from
  Students, Groups, Plan
where
  Students.GroupId = Groups.GroupId
  and Groups.GroupId = Plan.GroupId
  and Plan.CourseId = :CourseId
  and StudentId not in (
      select
        StudentId
      from
        Marks
      where
        Marks.CourseId = :CourseId
    )
