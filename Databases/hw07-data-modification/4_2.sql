update Marks
set Mark = coalesce(
  (
    select Mark
    from NewMarks
    where
      Marks.StudentId = NewMarks.StudentId
      and Marks.CourseId = NewMarks.CourseId
  ),
  Marks.Mark
)
