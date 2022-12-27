select CourseName, GroupName
from Courses
  natural join Groups
  natural join (select CourseId, GroupId
                from Marks
                  cross join Students
                where Mark >= 3
                except
                  select CourseId, GroupId
                  from
                     (select Students.StudentId, CourseId, GroupId
                      from
                        Marks
                        cross join Students
                      except
                        select StudentId, CourseId, GroupId
                        from
                          Marks
                          natural join Students) SubQuery) WithIds;
