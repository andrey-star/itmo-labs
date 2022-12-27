select StudentName, CourseName
from (select
        StudentId, StudentName, CourseName
      from
        Students
        natural join Plan
        natural join Courses
      except
        select StudentId, StudentName, CourseName
        from
          Students
          natural join Marks
          natural join Courses) WithStudentId;
