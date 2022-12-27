select distinct StudentId, StudentName, GroupId
from Students natural join Marks
where Mark = :Mark and CourseId = :CourseId;