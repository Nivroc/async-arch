CREATE TABLE asyncarch.ttusers (
    uuid        varchar(50) PRIMARY KEY, 
    fullname    varchar(25), 

    CONSTRAINT t UNIQUE (uuid)
);

CREATE TABLE asyncarch.tasks (
    uuid        varchar(50) PRIMARY KEY, 
    id          varchar(20),
    name        varchar(50), 
    description varchar(3000),
    open        boolean,
    assignee    varchar(50),

    CONSTRAINT t2 UNIQUE (uuid),
    CONSTRAINT t3 UNIQUE (id),
    CONSTRAINT t4 UNIQUE (name)
);
