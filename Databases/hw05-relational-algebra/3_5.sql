select StudentId, StudentName, Students.GroupId
from
  Students
  natural join Marks
  inner join Plan
    on Marks.CourseId = Plan.CourseId
where Mark = :Mark and LecturerId = :LecturerId;
