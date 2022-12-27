select distinct StudentId, StudentName, Students.GroupId
from Students, Groups
where Students.GroupId = Groups.GroupId and GroupName = :GroupName