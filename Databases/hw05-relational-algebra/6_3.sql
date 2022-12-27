select distinct StudentId
from Marks
except
  select StudentId
  from (select StudentId, CourseId
        from
          (select StudentId from Marks) PiXQ
           cross join (select CourseId
                       from
                         Plan
                         natural join Lecturers
                         where LecturerName = :LecturerName) S
        except
          select StudentId, CourseId
          from Marks) RightSide;
