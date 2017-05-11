CREATE TABLE IF NOT EXISTS
gs ( likes text
   , notlikes text
   , isvalid boolean
   , time datetime default
    (datetime('now', 'localtime')) );
