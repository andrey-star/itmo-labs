select distinct Students.StudentId, StudentName, GroupId
from Students, Marks, Courses
where Students.StudentId = Marks.StudentId
  and Marks.CourseId = Courses.CourseId
  and Mark = :Mark
  and CourseName = :CourseName
