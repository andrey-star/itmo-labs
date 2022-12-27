select distinct Students.StudentId
from
  Students,
  Lecturers,
  Plan,
  Marks
where
  Students.StudentId = Marks.StudentId
  and Students.GroupId = Plan.GroupId
  and Lecturers.LecturerId = Plan.LecturerId
  and Plan.CourseId = Marks.CourseId
  and LecturerName = :LecturerName
