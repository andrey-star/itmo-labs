select GroupName, SumMark
from Groups
left join (select GroupId, sum(Mark) as SumMark
           from Marks
           natural join Students
           group by GroupId) MarkGroupSum
on Groups.GroupId = MarkGroupSum.GroupId;
