select TeamName
from
  Teams natural join (
    select distinct TeamId
    from Sessions
    where ContestId = :ContestId
  ) SC
