CREATE TABLE asyncarch.taskcost (
    taskpid       varchar(50) PRIMARY KEY, 
    id            varchar(20),
    cost          int, 
    description   varchar(3000),
    open          boolean,
    assignee      varchar(50),

    CONSTRAINT a UNIQUE (uuid)
);

CREATE TABLE asyncarch.credit (
    pid             varchar(50) PRIMARY KEY, 
    id              varchar(20),
    userid          varchar(50), 
    description     varchar(3000),
    amount          int,
    ts              timestamp,
    period          int,
    open            boolean,

    CONSTRAINT a1 UNIQUE (pid),
    CONSTRAINT a2 UNIQUE (id),
    CONSTRAINT a3 UNIQUE (userid)
);

CREATE TABLE asyncarch.debit (
    pid             varchar(50) PRIMARY KEY, 
    id              varchar(20),
    userid          varchar(50), 
    description     varchar(3000),
    amount          int,
    ts              timestamp,
    period          int,
    open            boolean,

    CONSTRAINT a4 UNIQUE (uuid),
    CONSTRAINT a5 UNIQUE (userid),
    CONSTRAINT a6 UNIQUE (pid)
);

CREATE view asyncarch.auditlog as (...);
