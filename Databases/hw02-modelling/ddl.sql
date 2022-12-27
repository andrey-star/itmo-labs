create table Groups (
  id int not null primary key,
  name char(6) not null
);

create table Students (
  id int not null primary key,
  first_name varchar(30) not null,
  last_name varchar(30) not null,
  group_id int not null,
  constraint student_group_fk foreign key (group_id) references Groups (id)
);

create table Courses (
  id int not null primary key,
  name varchar(30) not null
);

create type mark as enum('FX', 'E', 'D', 'C', 'B', 'A');

create table Marks (
  student_id int not null,
  course_id int not null,
  value mark not null,
  constraint mark_pk primary key (student_id, course_id),
  constraint mark_student_fk foreign key (student_id) references Students (id),
  constraint mark_course_fk foreign key (course_id) references Courses (id)
);

create table Teachers (
  id int not null primary key,
  first_name varchar(30) not null,
  last_name varchar(30) not null
);

create table TeachersCourses (
  teacher_id int not null,
  course_id int not null,
  constraint teacher_course_pk primary key (teacher_id, course_id),
  constraint teacher_course_teacher_fk foreign key (teacher_id) references Teachers (id),
  constraint teacher_course_course_fk foreign key (course_id) references Courses (id)
);
