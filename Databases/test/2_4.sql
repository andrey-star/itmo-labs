existsAcceptedRun(SessionId, ContestId, Letter) :-
  Runs(_, SessionId, Letter, _, 1),
  Sessions(SessionId, _, ContestId, _).
existsSessionWhichDidntSolve(ContestId, Letter) :-
  Sessions(SessionId, _, ContestId, _),
  Problems(ContestId, Letter, _),
  not existsAcceptedRun(SessionId, ContestId, Letter).
r(ContestId, Letter) :-
  Problems(ContestId, Letter, _),
  not existsSessionWhichDidntSolve(ContestId, Letter).

select ContestId, Letter
from Problems
where not exists (
  select ContestId, Letter
  from (
    select Sessions, Problems
    where not exists (
      select *
      from Runs, Sessions
      where Sessions.ContestId = Problems.ContestId
        and Runs.Letter = Problems.Letter
        and Accepted = 1
        and Runs.SessionId = Sessions.SessionId
    )
  ) DS
  where Problems.ContestId = DS.ContestId
    and Problems.Letter = DS.Letter
)
