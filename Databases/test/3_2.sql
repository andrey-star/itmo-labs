delete from Runs
where SessionId in (
  select SessionId
  from Sessions
    natural join Contests
  where ContestName = :ContestName
)
