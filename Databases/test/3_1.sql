delete from Runs
where SessionId in (
  select SessionId
  from Sessions
  where TeamId = :TeamId
)
