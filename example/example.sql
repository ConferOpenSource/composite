create database example;

create user example;

grant all privileges on database example to example;

\c example

create type usertype as enum('Owner', 'Manager', 'Regular');
create sequence user_ids;

create table users (
  id integer primary key default nextval('user_ids'),
  login varchar not null,
  usertype usertype not null
);

alter sequence user_ids owner to example;
alter table users owner to example;

copy users (id, login, usertype) from stdin;
1	Alice	Owner
2	Bob	Manager
3	Cathy	Regular
\.
