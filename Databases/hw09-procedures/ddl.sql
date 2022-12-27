-- Таблица Flights из условия, дополнительных столбцов нет.
-- FlightId - PK
create table Flights
(
    FlightId   int primary key,
    FlightTime timestamp not null,
    PlaneId    int       not null
);

-- Таблица Seats из условия, дополнительных столбцов нет.
-- PlaneId + SeatNo - PK
create table Seats
(
    PlaneId int        not null,
    SeatNo  varchar(4) not null,
    constraint mark_pk primary key (PlaneId, SeatNo)
);

-- Таблица Users хранит id пользователей и захешированный пароль.
-- UserId - PK.
create table Users
(
    UserId int primary key,
    Pass   text not null
);

-- enum TicketType используется для опеределения типа билета в таблице
-- Tickets.
create type TicketType AS ENUM ('reserved', 'bought');

-- Таблица Tickets хранит совершенные покупки/резервации билетов на рейс.
-- Хранит пользователя, если была совершена резервация. Момент покупки и рейс с местом.
-- FlightId + SeatNo - PK.
create table Tickets
(
    FlightId     int        not null,
    SeatNo       varchar(4) not null,
    UserId       int,
    constraint ticket_pk primary key (FlightId, SeatNo),
    Status       TicketType not null,
    PurchaseTime timestamp  not null
);

-- Представление, оставляющие среди покупок только действительные,
-- а именно купленные, а также зарезервированные не ранее 3 дней
-- с текущего момента.
create view ReservedTickets as
select FlightId, SeatNo, UserId, Status, PurchaseTime
from Tickets
where Status = 'bought'
   or PurchaseTime >= now() - interval '3 days';

-- Функция, валидирующая покупку/резервацию места, проверяя,
-- что в самолете данного рейса существует такое место.
-- Потребуется в определении последующего триггера.
create or replace function validate_ticket()
    returns trigger
    language plpgsql
as
$$
begin
    if new.SeatNo not in (
        select SeatNo
        from Flights
                 natural join Seats
        where Flights.FlightId = new.FlightId
    ) then
        raise exception 'seat doesnt exist';
    end if;
    return NEW;
end;
$$;

-- Триггер, не позволяющий купить/забронировать место,
-- которого не существует в самолете данного рейса.
create trigger NoTicketsWithoutSeat
    before insert or update
    on Tickets
    for each row
execute procedure validate_ticket();
