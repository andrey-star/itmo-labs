select TotalQuery.StudentId, Total, coalesce(Passed, 0) as Passed, Total - coalesce(Passed, 0) as Failed
from (select StudentId, coalesce(count(distinct CourseId), 0) as Total
      from
        Students
        left join Plan
          on Students.GroupId = Plan.GroupId
      group by Students.StudentId) TotalQuery
  left join (select Students.StudentId, count(distinct Marks.CourseId) as Passed
                from
                  Marks
                  natural join Plan
                  natural join Students
                group by Students.StudentId) PassedQuery
    on TotalQuery.StudentId = PassedQuery.StudentId;