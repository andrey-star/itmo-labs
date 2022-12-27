select StudentId, StudentName, GroupId
from
  Students
  natural join Plan
  natural join Marks
where Mark = :Mark and LecturerId = :LecturerId;
