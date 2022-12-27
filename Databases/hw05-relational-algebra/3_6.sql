select
  StudentId, StudentName, Students.GroupId
from
  Students
  natural join Marks
  inner join Plan
    on Marks.CourseId = Plan.CourseId
  inner join Lecturers
    on Plan.LecturerId = Lecturers.LecturerId
where Mark = :Mark and LecturerName = :LecturerName;
