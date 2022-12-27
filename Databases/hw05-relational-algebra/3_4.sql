select StudentId, StudentName, GroupId
from
  Students
  natural join Plan
  natural join Marks
  natural join Lecturers
where Mark = :Mark and LecturerName = :LecturerName;
