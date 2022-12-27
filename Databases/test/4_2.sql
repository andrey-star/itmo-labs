select TeamId, count(Letter) as Opened
from (
  select distinct TeamId, ContestId, Letter
  from Runs
    natural join Sessions
) DS
group by TeamId;
