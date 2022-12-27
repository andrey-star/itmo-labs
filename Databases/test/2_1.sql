select distinct TeamId
from (
  select SessionId, TeamId
  from Sessions
  where ContestId = :ContestId
    and SessionId in (
      select SessionId
      from Runs
      where Letter = :Letter
        and Accepted = 0
    )
) DS
