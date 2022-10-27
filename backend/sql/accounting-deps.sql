CREATE TABLE asyncarch.acusers (
    uuid        UUID PRIMARY KEY, 
    roles       varchar(10)[],
    fullname    varchar(25), 
    email       varchar(50),

    CONSTRAINT a0 UNIQUE (uuid)
);

CREATE TABLE asyncarch.taskcost (
    uuid          UUID PRIMARY KEY, 
    title         varchar(20),
    jira_id       varchar(50), 
    cost          int, 
    reward        int,
    description   varchar(3000),
    open          boolean,
    assignee      UUID,

    CONSTRAINT a UNIQUE (uuid)
);


CREATE OR REPLACE VIEW asyncarch.auditlog as (
    SELECT uuid, title, jira_id, userid, description, -amount, ts from asyncarch.credit
    union all
    SELECT * from asyncarch.debit
    order by ts desc
);

CREATE TABLE asyncarch.credit (
    uuid            UUID PRIMARY KEY, 
    title           varchar(20),
    jira_id         varchar(50), 
    userid          UUID, 
    description     varchar(3000),
    amount          int,
    ts              timestamp,

    CONSTRAINT a1 UNIQUE (uuid)
);

CREATE TABLE asyncarch.debit (
    uuid            UUID PRIMARY KEY, 
    title           varchar(20),
    jira_id         varchar(50), 
    userid          UUID, 
    description     varchar(3000),
    amount          int,
    ts              timestamp,

    CONSTRAINT a4 UNIQUE (uuid)
);

CREATE view asyncarch.auditlog as (...);
