update Runs
set Accepted = 1
where RunId in (
  select RunId
  from Runs
    natural join (
      select SessionId, max(SubmitTime) as SubmitTime
      from Runs
      where Accepted = 0
      group by SessionId
  ) DS
)
