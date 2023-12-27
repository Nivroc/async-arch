CREATE TABLE asyncarch.tasksv2 (
    uuid        varchar(50) PRIMARY KEY, 
    title       varchar(20),
    jira_id     varchar(50), 
    description varchar(3000),
    open        boolean,
    assignee    varchar(50),

    CONSTRAINT t2 UNIQUE (uuid),
    CONSTRAINT t4 UNIQUE (jira_id)
);

BEGIN;
Lock table asyncarch.tasksv1 IN ACCESS SHARE MODE;
insert into asyncarch.tasksv2 as (
    select 
        uuid, 
        case
            when substring(jira_id, '$"[%]$"', '$') = '' then '[]'
            else substring(jira_id, '$"[%]$"', '$')
        end as title,    
        substring(jira_id, '[%]$"%$"', '$') as jira_id,
        description,
        open,
        assignee
    from asyncarch.tasksv1    
);
COMMIT;
