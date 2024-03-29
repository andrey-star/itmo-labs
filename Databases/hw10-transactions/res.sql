-- FreeSeats
start transaction read only isolation level read committed;
-- Данные не изменяются, значит 'косая запись' не помешает.
-- 'Фантомная запись' и 'неповторяемое чтение' так же не помешают,
-- т. к. читаем один раз.
-- 'Read uncommited' недостаточно, т.к. параллельно другой
-- пользователь может быть в процессе бронирования, которое
-- впоследствии отменит. Тогда текущему пользователю отобразят место
-- как забронированное, хотя он мог бы купить на него билет. Поэтому,
-- несмотря на частое изменение свободных мест, 'read uncommited' не
-- подходит (однако в зависимости от целей read uncommited может быть
-- достаточен).

-- Reserve
start transaction read write isolation level repeatable read;
-- Работаем с одной записью, поэтому 'косая запись' и 'фантомная
-- запись' на помешают. 'Неповторяемое чтение' возможно при
-- исполнении двух транзакций - обоим вернется, что ответ записан,
-- однако на самом деле один ответ перезаписал другой. Поэтому
-- возьмем на нее блокировку с помощью 'repeatable read'.
-- Для запроса удаления также подойдет 'repeatable read'.


-- ExtendReservation
start transaction read write isolation level read committed;
-- 'Read uncommited 'недостаточно, т.к. изменяем данные.
-- Смотрим на одно конкретное бронирование, поэтому 'косая запись' и
-- 'фантомная запись' не помешают. 'неповторяемое чтение' может
-- возникнуть при параллельном продлении резервации одного места,
-- однако в таком случае оба запроса происходят примерно в один
-- момент и итоговый момент брони будет почти идентичным в обоих
-- случаях, значит 'repeatable read' не требуется. Значит 'read
-- commited' достаточно.

-- BuyFree
start transaction read write isolation level repeatable read;
-- Работаем с одной записью, поэтому 'косая запись' и
-- 'фантомная запись' на помешают. 'Неповторяемое чтение' возможно
-- при исполнении двух транзакций - обоим вернется, что ответ
-- записан, однако на самом деле один ответ перезаписал другой.
-- Поэтому возьмем на нее блокировку с помощью 'repeatable read'.
-- Для запроса удаления также подойдет 'repeatable read'.

-- BuyReserved
start transaction read write isolation level read committed;
-- Похоже на ExtendReservation, просто дополнительно меняется статус
-- места с 'reserved' на 'bought'.
-- 'Read uncommited 'недостаточно, т.к. изменяем данные.
-- Смотрим на одно конкретное бронирование, поэтому 'косая запись' и
-- 'фантомная запись' не помешают. 'неповторяемое чтение' может
-- возникнуть при параллельной покупке одного места, однако в таком
-- случае оба запроса происходят примерно в один момент и с одного
-- аккаунта, пожтому неважно, какой завершится первым, какой вторым,
-- значит 'repeatable read' не требуется. Значит 'read commited'
-- достаточно.

-- FlightStatistics
start transaction read only isolation level read committed;
-- Статистический запрос, результат которого все может сразу
-- устареть. Однако требуется авторизация, поэтому 'read committed'.

-- FlightStat
start transaction read only isolation level read committed;
-- Статистический запрос, результат которого все может сразу
-- устареть. Однако требуется авторизация, поэтому 'read committed'.

-- CompressSeats
start transaction read write isolation level snapshot;
-- Исполнение должно запретить покупку/резервацию билетов. Поэтому
-- требуется уровень изоляции не ниже 'snapshot'. При этом 'косая
-- запись' не возникает, значит 'serializable' не требуется.


-- 2.0. Общий план
Взаимодействие будет реализовано через веб-приложение.
После загрузки сайта пользователь может ввести номер рейса или выбрать его из списка всех рейсов. После выбора загружается страница со списком свободных мест на данном рейсе, используя запрос FreeSeats.
Пользователь может выбрать место и перейти к покупке/брони. Бронь/выкуп брони требует авторизации, поэтому на сайте будет возможность войти в свой аккаунт. Если пользователь попробует забронировать место, предварительно не авторизировавшись, ему предложат это сделать. Дальше пользователь может купить/забронировать место и в случае успеха ему будут сообщены детали. В случае неудачи будет предложено вернуться и купить другое место. Активные брони и покупки можно будет посмотреть в личном кабинете.

-- 2.1. Запрос списка свободных мест
start transaction isolation level read committed;
select FreeSeats(:FlightId);
commit;

-- 2.2. Взаимодействие с пользователем
При загрузке сайта есть поле для ввода номера рейса + выпадающий список всех рейсов или фильтрованный список по аэропорту/городу отправки/прилета. При нажатии кнопки для введенного рейса отображается список свободных мест. Если пользователь не авторизирован - кнопка бронирования/выкупа брони будет отключена и предлагается войти в аккаунт. Для входа пользователь вводит логин и пароль. После этого у него появляется возможность купить билет. В случае успеха ему будут сообщены детали. В случае неудачи будет предложено вернуться и купить другое место. Активные брони и покупки можно будет посмотреть в личном кабинете.

-- 2.3. Действия с местом
start transaction read write isolation level repeatable read;
select Reserve(:UserId, :Pass, :FlightId, :SeatNo);
commit;

start transaction read write isolation level read committed;
select BuyReserved(:UserId, :Pass, :FlightId, :SeatNo);
commit;

start transaction read write isolation level repeatable read;
select BuyFree(:FlightId, :SeatNo);
commit;
