# Атрибуты

```
StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark
```

# 1. Функциональные зависимости

-- По идентификатору студента можно определить его имя и какой группе он принадлежит
StudentId -> StudentName, GroupId
-- По идентификатору группы можно определить ее название
GroupId -> GroupName
-- По идентификатору курса можно определить его название
CourseId -> CourseName
-- По идентификатору лектора можно определить его имя
LecturerId -> LecturerName
-- По идентификатору группы и курса можно определить лектора, который читает данный курс данной группе
GroupId, CourseId -> LecturerId
-- По идентификатору студента и курса можно определить оценку этого студента по этому курсу
StudentId, CourseId -> Mark
-- По названию группы можно определить ее идентификатор (тк названия групп уникальны)
GroupName -> GroupId

# Ключи

## 2.1 Процесс определения ключей

1. StudentId и CourseId не встречаются в правых частях ФЗ, значит однозначно являются частью искомого ключа (отсутствие атрибута в правых частях означает невозможность вывести этот атрибут).

2. Теперь покажем, что {StudentId, CourseId} это надключ. Замкнув {StudentId, CourseId} над множеством ФЗ мы получим полное мн-во атрибутов (подробно это показано в пункте 3.2 данного ДЗ). Из этого следует, что {StudentId, CourseId} - надключ.

3. Из 2 следует, что {StudentId, CourseId} - надключ, из 1 следует, что данный надключ - минимальный по включению. Следовательно {StudentId, CourseId} - ключ.

4. Других ключей нет, так как любой другой ключ будет содержать {StudentId, CourseId} (из 1), а значит либо является найденным ранее ключом, либо включает дополнительные атрибуты, а значит не является минимальным по включению, то есть не является ключом.

## 2.2 Полученные ключи

```
{StudentId, CourseId}
```

# 3. Замыкания множества атрибутов

## 3.1 GroupId, CourseId

```
{GroupId, CourseId}
{GroupId, GroupName, CourseId}
{GroupId, GroupName, CourseId, CourseName}
{GroupId, GroupName, CourseId, CourseName, LecturerId}
{GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName}
```

## 3.2 StudentId, CourseId

```
{StudentId, CourseId}
{StudentId, StudentName, CourseId}
{StudentId, StudentName, CourseId, CourseName}
{StudentId, StudentName, CourseId, CourseName, GroupId}
{StudentId, StudentName, CourseId, CourseName, GroupId, Mark}
{StudentId, StudentName, CourseId, CourseName, GroupId, GroupName, LecturerId, Mark}
{StudentId, StudentName, CourseId, CourseName, GroupId, GroupName, LecturerId, LecturerName, Mark}
```

## 3.2 StudentId, LecturerId

```
{StudentId, LecturerId}
{StudentId, StudentName, LecturerId}
{StudentId, StudentName, LecturerId, LecturerName}
{StudentId, StudentName, GroupId, LecturerId, LecturerName}
{StudentId, StudentName, GroupId, GroupName, LecturerId, LecturerName}
```

# 4. Неприводимое множество функциональных зависимостей

## 4.1d. Первый этап

Расщепление правых частей. Расщепление возможно только для первого правила: StudentId -> StudentName GroupId переходит в два отдельных StudentId -> StudentName, StudentId -> GroupId

## 4.1r. Результаты первого этапа

```
StudentId -> StudentName
StudentId -> GroupId
GroupId -> GroupName
CourseId -> CourseName
LecturerId -> LecturerName
GroupId, CourseId -> LecturerId
StudentId, CourseId -> Mark
GroupName -> GroupId
```

## 4.2d. Второй этап

Удаление атрибута из левой части. Среди ФЗ, левые части которых состоят из более, чем одного элемента, пробуем удалить каждый из атрибутов. Затем проверяем, содержит ли замыкание атрибутов из оставшейся левой части над исходным множеством ФЗ правую часть. Если нет, удалять атрибут нельзя. Если содержит, то можно удалить, однако следует рассмотреть остальные возможности удаления из исходного правила. Тем самым одна ФЗ может породить несколько ФЗ на этом этапе.

Есть два правила, подлежащих проверке:

1. GroupId CourseId -> LecturerId

   1. GroupId -> LecturerId (удалили CourseId)
      Замкнем {GroupId} над изначальным множеством ФЗ.

      ```
      {GroupId}
      {GroupId, GroupName}
      ```

      LecturerId отсутствует в замыкании, следовательно удалить CourseId нельзя.

   2. CourseId -> LecturerId (удалили GroupId)
      Замкнем {CourseId} над изначальным множеством ФЗ.
      ```
      {CourseId}
      {CourseId, CourseName}
      ```
      LecturerId отсутствует в замыкании, следовательно удалить GroupId нельзя.

2. StudentId CourseId -> Mark

   1. StudentId -> Mark (удалили CourseId)
      Замкнем {StudentId} над изначальным множеством ФЗ.

      ```
      {StudentId}
      {StudentId, StudentName, GroupId}
      {StudentId, StudentName, GroupId, GroupName}
      ```

      Mark отсутствует в замыкании, следовательно удалить CourseId нельзя.

   2. CourseId -> Mark (удалили StudentId)
      Замкнем {CourseId} над изначальным множеством ФЗ.
      ```
      {CourseId}
      {CourseId, CourseName}
      ```
      Mark отсутствует в замыкании, следовательно удалить StudentId нельзя.

## 4.2r. Результаты второго этапа

```
StudentId -> StudentName
StudentId -> GroupId
GroupId -> GroupName
CourseId -> CourseName
LecturerId -> LecturerName
GroupId, CourseId -> LecturerId
StudentId, CourseId -> Mark
GroupName -> GroupId
```

## 4.3d. Третий этап

Удаление правила. Пробуем удалить правило X -> Y, затем проверяем, принадлежит ли Y замыканию X над изначальным множеством ФЗ без удаленного правила. Если да, то правило можно удалить.

1. Удаляем StudentId -> StudentName
   Замыкаем {StudentId} над оставшимися правилами.

   ```
   {StudentId}
   {StudentId, GroupId}
   {StudentId, GroupId, GroupName}
   ```

   StudentName отсутствует в замыкании, следовательно удалить правило нельзя.

2. Удаляем StudentId -> GroupId
   Замыкаем {StudentId} над оставшимися правилами.

   ```
   {StudentId}
   {StudentId, StudentName}
   ```

   GroupId отсутствует в замыкании, следовательно удалить правило нельзя.

3. Удаляем GroupId -> GroupName
   Замыкаем {GroupId} над оставшимися правилами.
   {GroupId}
   GroupName отсутствует в замыкании, следовательно удалить правило нельзя.

4. Удаляем CourseId -> CourseName
   Замыкаем {CourseId} над оставшимися правилами.

   ```
   {CourseId}
   ```

   CourseName отсутствует в замыкании, следовательно удалить правило нельзя.

5. Удаляем LecturerId -> LecturerName
   Замыкаем {LecturerId} над оставшимися правилами.

   ```
   {LecturerId}
   ```

   LecturerName отсутствует в замыкании, следовательно удалить правило нельзя.

6. Удаляем GroupId CourseId -> LecturerId
   Замыкаем {GroupId, CourseId} над оставшимися правилами.

   ```
   {GroupId, CourseId}
   {GroupId, GroupName, CourseId, CourseName}
   ```

   LecturerId отсутствует в замыкании, следовательно удалить правило нельзя.

7. Удаляем StudentId CourseId -> Mark
   Замыкаем {StudentId, CourseId} над оставшимися правилами.

   ```
   {StudentId, CourseId}
   {StudentId, StudentName, GroupId, CourseId, CourseName}
   {StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId}
   {StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName}
   ```

   Mark отсутствует в замыкании, следовательно удалить правило нельзя.

8. Удаляем GroupName -> GroupId
   Замыкаем `{GroupName}` над оставшимися правилами.
   ```
   {GroupName}
   ```
   GroupId отсутствует в замыкании, следовательно удалить правило нельзя.

Ни одно правило удалить нельзя.

## 4.3r. Результаты третьего этапа

```
StudentId -> StudentName
StudentId -> GroupId
GroupId -> GroupName
CourseId -> CourseName
LecturerId -> LecturerName
GroupId, CourseId -> LecturerId
StudentId, CourseId -> Mark
GroupName -> GroupId
```
