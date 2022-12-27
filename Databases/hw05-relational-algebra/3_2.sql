select StudentId, StudentName, GroupId
from
  Students
  natural join Marks
  natural join Courses
where Mark = :Mark and CourseName = :CourseName;
