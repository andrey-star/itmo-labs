create table Groups (
  id int not null primary key,
  group_name char(6) not null unique
);

create table Students (
  id int not null primary key,
  student_name varchar(30) not null,
  group_id int not null,
  constraint student_group_fk foreign key (group_id) references Groups (id)
);

create table Courses (
  id int not null primary key,
  course_name varchar(30) not null
);

create table Marks (
  student_id int not null,
  course_id int not null,
  mark char not null,
  constraint mark_pk primary key (student_id, course_id),
  constraint mark_student_fk foreign key (student_id) references Students (id),
  constraint mark_course_fk foreign key (course_id) references Courses (id)
);

create table Lecturers (
  id int not null primary key,
  lecturer_name varchar(30) not null
);

create table GroupCourseLecturers (
  group_id int not null,
  course_id int not null,
  lecturer_id int not null,
  constraint group_course_lecturer_pk primary key (group_id, course_id),
  constraint group_course_lecturer_group_fk foreign key (group_id) references Groups (id),
  constraint group_course_lecturer_course_fk foreign key (course_id) references Courses (id),
  constraint group_course_lecturer_lecturer_fk foreign key (lecturer_id) references Lecturers (id)
);
