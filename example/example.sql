create database example;

create user example;

grant all privileges on database example to example;

\c example

create type usertype as enum('Owner', 'Manager', 'Regular');

create table users (
  id bigint not null primary key,
  login varchar not null,
  usertype usertype not null
);

alter table users owner to example;

copy users (id, login, usertype) from stdin;
1	Alice	Owner
2	Bob	Manager
3	Cathy	Regular
\.

