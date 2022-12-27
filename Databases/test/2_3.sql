select distinct TeamId
from (
  select SessionId, TeamId
  from Sessions
  where ContestId = :ContestId
    and SessionId in (
      select SessionId
      from Runs
      where Accepted = 0
    )
) DS
