CREATE TABLE asyncarch.users (
    uuid varchar(50), 
    login varchar(25) PRIMARY KEY, 
    email varchar(50), 
    secret varchar(50), 
    roles varchar(10)[], 

    CONSTRAINT u1 UNIQUE (uuid),
    CONSTRAINT u1 UNIQUE (login)
    CONSTRAINT u1 UNIQUE (email)
);

CREATE TABLE asyncarch.blctokens (
    token Text PRIMARY KEY, 

    CONSTRAINT u unique (token)
);