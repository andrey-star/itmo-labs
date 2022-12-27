select SessionId, count(Letter) as Opened
from (
  select distinct SessionId, Letter
  from Runs
) DS
group by SessionId;
