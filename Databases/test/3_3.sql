insert into Sessions (TeamId, ContestId, Start)
select TeamId, :ContestId, current_timestamp
from Sessions
where ContestId = :ContestId
