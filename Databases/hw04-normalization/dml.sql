insert into Groups
  (id, group_name) values
  (0, 'M34371'),
  (1, 'M34391');

insert into Students
  (id, student_name, group_id) values
  (0, 'Алексей Михайлов', 0),
  (1, 'Марат Данилов', 0),
  (2, 'Григорий Иванов', 1);

insert into Courses
  (id, course_name) values
  (0, 'Теория информации'),
  (1, 'Математический анализ'),
  (2, 'Анализ данных'),
  (3, 'История');

 insert into Marks
   (student_id, course_id, mark) values
   (0, 0, 'A'),
   (0, 1, 'B'),
   (0, 2, 'C'),
   (1, 1, 'D'),
   (1, 2, 'E'),
   (1, 3, 'F'),
   (2, 0, 'A'),
   (2, 3, 'B');

insert into Lecturers
  (id, lecturer_name) values
  (0, 'Михаил Алексеев'),
  (1, 'Даниил Маратов'),
  (2, 'Иван Григорьев');

insert into GroupCourseLecturers
  (group_id, course_id, lecturer_id) values
  (0, 0, 0),
  (0, 1, 1),
  (1, 2, 1),
  (1, 3, 2);
