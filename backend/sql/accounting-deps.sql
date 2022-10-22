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

CREATE TABLE asyncarch.credit (
    uuid            UUID PRIMARY KEY, 
    title           varchar(20),
    jira_id         varchar(50), 
    userid          UUID, 
    description     varchar(3000),
    amount          int,
    ts              timestamp,

    CONSTRAINT a1 UNIQUE (pid),
    CONSTRAINT a2 UNIQUE (id),
    CONSTRAINT a3 UNIQUE (userid)
);

CREATE TABLE asyncarch.debit (
    uuid            UUID PRIMARY KEY, 
    title           varchar(20),
    jira_id         varchar(50), 
    userid          UUID, 
    description     varchar(3000),
    amount          int,
    ts              timestamp,

    CONSTRAINT a4 UNIQUE (uuid),
    CONSTRAINT a5 UNIQUE (userid),
    CONSTRAINT a6 UNIQUE (pid)
);

CREATE view asyncarch.auditlog as (...);
