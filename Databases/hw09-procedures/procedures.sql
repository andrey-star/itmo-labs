-- Требуется для вызова функции crypt
create extension pgcrypto;

-- Возвращает true, если данный пользователь существует и передал верный пароль.
-- Иначе возвращает false.
create
    or replace function Auth(user_id int, password text)
    returns boolean
    language plpgsql
as
$$
declare
    did_match boolean := (
        select (pass = crypt(password, pass)) as pswmatch
        from Users
        where UserId = user_id
    );
begin
    return coalesce(did_match, false);
end;
$$;

-- Регистрирует пользователя, если пользователя с таким id еще нет,
-- добавляя его в таблицу Users. Хеширует переданный пароль с солью.
-- Возвращает true при удачной регистрации, false иначе.
create
    or replace function Register(user_id int, password text)
    returns boolean
    language plpgsql
as
$$
begin
    if user_id in (select UserId from Users)
    then
        return false;
    end if;
    insert into Users(UserId, Pass) values (user_id, crypt(password, gen_salt('md5')));
    return true;
end;
$$;

create or replace function FreeSeats(flight_id int)
    returns table
            (
                FreeSeatNo varchar(4)
            )
    language plpgsql
as
$$
declare
    plane_id int := (select PlaneId
                     from Flights
                     where FlightId = flight_id);
begin
    return query (
        select SeatNo as FreeSeatNo
        from Seats
        where PlaneId = plane_id
          and SeatNo not in (
            select SeatNo
            from ReservedTickets
            where FlightId = flight_id
        )
    );
end;
$$;



create
    or replace function Reserve(user_id int, password text, flight_id int, seat_no varchar(4))
    returns boolean
    language plpgsql
as
$$
declare
    flight_time timestamp := (select FlightTime
                              from Flights
                              where FlightId = flight_id);
begin
    if flight_time < now()
        or not Auth(user_id, password)
        or seat_no in (
            select SeatNo
            from ReservedTickets
            where FlightId = flight_id)
    then
        return false;
    end if;

    delete from Tickets where FlightId = flight_id and SeatNo = seat_no;
    insert into Tickets(FlightId, SeatNo, UserId, Status, PurchaseTime)
    values (flight_id, seat_no, user_id, 'reserved', now());
    return true;
end;
$$;

create
    or replace function ExtendReservation(user_id int, password text, flight_id int, seat_no varchar(4))
    returns boolean
    language plpgsql
as
$$
declare
    flight_time timestamp := (select FlightTime
                              from Flights
                              where FlightId = flight_id);
begin
    if flight_time < now()
        or not Auth(user_id, password)
        or seat_no not in (
            select SeatNo
            from ReservedTickets
            where FlightId = flight_id
              and UserId = user_id
              and Status = 'reserved')
    then
        return false;
    end if;

    update Tickets
    set PurchaseTime = now()
    where FlightId = flight_id
      and SeatNo = seat_no;
    return true;
end;
$$;

create
    or replace function BuyFree(flight_id int, seat_no varchar(4))
    returns boolean
    language plpgsql
as
$$
declare
    flight_time timestamp := (select FlightTime
                              from Flights
                              where FlightId = flight_id);
begin
    if flight_time < now()
        or seat_no in (
            select SeatNo
            from ReservedTickets
            where FlightId = flight_id)
    then
        return false;
    end if;

    delete from Tickets where FlightId = flight_id and SeatNo = seat_no;
    insert into Tickets(FlightId, SeatNo, Status, PurchaseTime)
    values (flight_id, seat_no, 'bought', now());
    return true;
end;
$$;

create
    or replace function BuyReserved(user_id int, password text, flight_id int, seat_no varchar(4))
    returns boolean
    language plpgsql
as
$$
declare
    flight_time timestamp := (select FlightTime
                              from Flights
                              where FlightId = flight_id);
begin
    if flight_time < now()
        or not Auth(user_id, password)
        or seat_no not in (
            select SeatNo
            from ReservedTickets
            where FlightId = flight_id
              and UserId = user_id
              and Status = 'reserved')
    then
        return false;
    end if;

    update Tickets
    set PurchaseTime = now(),
        Status       = 'bought'
    where FlightId = flight_id
      and SeatNo = seat_no;
    return true;
end;
$$;

create
    or replace function FlightsStatistics(user_id int, password text)
    returns table
            (
                FlightId   int,
                CanReserve boolean,
                CanBuy     boolean,
                Free       int,
                Reserved   int,
                Purchased  int
            )
    language plpgsql
as
$$
begin
    if not Auth(user_id, password)
    then
        return;
    end if;
    return query (
        select _FlightId  as FlightId,
               case
                   when FlightTime < now() then false
                   else _Total - _Reserved - _Purchased > 0
                   end    as CanReserve,
               case
                   when FlightTime < now() then false
                   else _Total - _Reserved - _Purchased > 0
                       or ReservedUser > 0
                   end    as CanBuy,
               case
                   when FlightTime < now() then 0
                   else _Total - _Reserved - _Purchased
                   end    as Free,
               _Reserved  as Reserved,
               _Purchased as Purchased
        from (
                 select Flights.FlightId                                 as _FlightId,
                        FlightTime,
                        count(*) filter (
                            where Status = 'reserved'
                                and UserId = user_id)                    as ReservedUser,
                        count(*)::int                                    as _Total,
                        count(*) filter (where Status = 'reserved')::int as _Reserved,
                        count(*) filter (where Status = 'bought')::int   as _Purchased
                 from Seats
                          natural join Flights
                          left join ReservedTickets using (FlightId, SeatNo)
                 group by Flights.FlightId) as Q);
end;
$$;

create
    or replace function FlightStat(user_id int, password text, flight_id int)
    returns table
            (
                FlightId   int,
                CanReserve boolean,
                CanBuy     boolean,
                Free       int,
                Reserved   int,
                Purchased  int
            )
    language plpgsql
as
$$
begin
    if not Auth(user_id, password)
    then
        return;
    end if;
    return query (
        select _FlightId  as FlightId,
               case
                   when FlightTime < now() then false
                   else _Total - _Reserved - _Purchased > 0
                   end    as CanReserve,
               case
                   when FlightTime < now() then false
                   else _Total - _Reserved - _Purchased > 0
                       or ReservedUser > 0
                   end    as CanBuy,
               case
                   when FlightTime < now() then 0
                   else _Total - _Reserved - _Purchased
                   end    as Free,
               _Reserved  as Reserved,
               _Purchased as Purchased
        from (
                 select Flights.FlightId                                 as _FlightId,
                        FlightTime,
                        count(*) filter (
                            where Status = 'reserved'
                                and UserId = user_id)                    as ReservedUser,
                        count(*)::int                                    as _Total,
                        count(*) filter (where Status = 'reserved')::int as _Reserved,
                        count(*) filter (where Status = 'bought')::int   as _Purchased
                 from Seats
                          natural join Flights
                          left join ReservedTickets using (FlightId, SeatNo)
                 where Flights.FlightId = flight_id
                 group by Flights.FlightId) as Q);
end;
$$;
