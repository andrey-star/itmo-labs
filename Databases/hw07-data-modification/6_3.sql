-- PostgreSQL 13.4
create trigger PreserverMarks before update or delete on Marks
for each row
execute procedure validate();

create or replace function validate()
  returns trigger
  language PLPGSQL
  as
$$
begin
  if NEW.MARK is null or OLD.Mark > NEW.MARK then
    raise exception 'cannot place mark';
  end if;
  return NEW;
end;
$$;
