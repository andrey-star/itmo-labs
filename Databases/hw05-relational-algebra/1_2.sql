select distinct StudentId, StudentName, GroupId
from Students
where StudentName = :StudentName;
