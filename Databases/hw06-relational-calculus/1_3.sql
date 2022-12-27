select distinct Students.StudentId, StudentName, GroupId
from Students, Marks
where Students.StudentId = Marks.StudentId
  and Mark = :Mark
  and CourseId = :CourseId
