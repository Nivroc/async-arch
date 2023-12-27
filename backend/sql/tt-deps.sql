CREATE TABLE asyncarch.ttusers (
    uuid        UUID PRIMARY KEY, 
    roles       varchar(10)[],
    fullname    varchar(25), 

    CONSTRAINT t UNIQUE (uuid)
);

CREATE TABLE asyncarch.tasksv2 (
    uuid        UUID PRIMARY KEY, 
    title       varchar(20),
    jira_id     varchar(50), 
    description varchar(3000),
    open        boolean,
    assignee    UUID,

    CONSTRAINT tt2 UNIQUE (uuid),
    CONSTRAINT tt4 UNIQUE (jira_id)
);

CREATE TABLE asyncarch.tasksv1 (
    uuid        UUID PRIMARY KEY, 
    jira_id     varchar(50), 
    description varchar(3000),
    open        boolean,
    assignee    UUID,

    CONSTRAINT t2 UNIQUE (uuid),
    CONSTRAINT t4 UNIQUE (jira_id)
);
