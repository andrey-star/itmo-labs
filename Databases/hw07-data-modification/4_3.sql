update Marks
set Mark = coalesce((
  select
    case
      when Marks.Mark > NewMarks.Mark then Marks.Mark
      else NewMarks.Mark
    end FinalMark
  from NewMarks
  where
    Marks.StudentId = NewMarks.StudentId
    and Marks.CourseId = NewMarks.CourseId
), Marks.Mark)
