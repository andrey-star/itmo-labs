select GroupName, AvgAvgMark
from Groups
left join (select GroupId, avg(cast(AvgMark as float)) as AvgAvgMark
           from
             (select
                StudentId, avg(cast(Mark as float)) as AvgMark
              from Marks
              group by StudentId) MarkStudentAvg
              natural join Students
           group by GroupId) MarkGroupAvg
on Groups.GroupId = MarkGroupAvg.GroupId;
