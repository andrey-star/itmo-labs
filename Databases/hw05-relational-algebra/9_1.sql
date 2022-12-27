select avg(cast(Mark as float)) as AvgMark
from Marks
where StudentId = :StudentId;
