-- PostgreSQL 13.4
create trigger NoExtraMarks before insert or update on Marks
for each row
execute procedure validate_marks();

create or replace function validate_marks()
  returns trigger
  language PLPGSQL
  as
$$
begin
  if not exists (
    select StudentId, CourseId
    from
      Students
      natural join Plan
    where
      Students.StudentId = NEW.StudentId
      and Plan.CourseId = NEW.CourseId
  ) then
    raise exception 'cannot place mark: no course in plan for given student';
  end if;
  return NEW;
end;
$$;

create trigger NoExtraMarksStudent before update or delete on Students
for each row
execute procedure validate_students();

create or replace function validate_students()
  returns trigger
  language PLPGSQL
  as
$$
begin
  if (NEW.StudentId is null or OLD.StudentId != NEW.StudentId) and exists (
    select *
    from
      Marks
    where
      Marks.StudentId = OLD.StudentId
  ) then
    raise exception 'cannot update student: marks without student would appear';
  end if;
  if OLD.StudentId = NEW.StudentId and OLD.GroupId != NEW.GroupId and exists (
    select *
    from
      Marks
    where
      Marks.StudentId = NEW.StudentId
      and StudentId not in (
        select StudentId
        from
          Plan
        where
          Plan.GroupId = NEW.GroupId
          and Plan.CourseId = Marks.CourseId
      )
  ) then
    raise exception 'cannot update student: group transfer produces orphan marks';
  end if;
  return NEW;
end;
$$;

create trigger NoExtraMarksPlan before update or delete on Plan
for each row
execute procedure validate_plan();

create or replace function validate_plan()
  returns trigger
  language PLPGSQL
  as
$$
begin
  if (NEW.CourseId != OLD.CourseId or NEW.GroupId != OLD.GroupId or NEW.LecturerId is null) and exists (
    select *
    from
      Marks
      natural join Students
      where
        Students.GroupId = OLD.GroupId
        and Marks.CourseId = OLD.CourseId
  ) then
    raise exception 'cannot update plan: plan update produces orphan marks';
  end if;
  return NEW;
end;
$$;
