eqlite
=====

An OTP library to assist with SQL code management.  The idea (though
not the implementation) was taken from the eql library.

Use
-----

`eqlite:init(Directory)` will suss out *.eqlite files recursively
starting with the director specified.  An appropriate invocation might
be something like `eqlite:init(code:priv_dir(my_app))`.  Rerunning
init will update entries as necessary but will not discard stale
entries.

`eqlite:get_query(QueryName)` will return the string text of the named
query or `undefined` if not found.

`eqlite:get_info(QueryName)` will return the info string of the named
query of `undefined` if not found.

`eqlite:list_queries()` will return a lsit of (atom) query names.

File Structure
-----

SQL queries beging with (and are delimited by) the sequence `-- :` as
in

``` sql
--
-- Some comment here.
-- :get_all_users
SELECT *
FROM users

-- Another comment.
-- :delete_all_users Deletes all users from the user table.
TRUNCATE TABLE users

```

Note that `-- :query_name` must start in column one!  Any text
following the query_name is considered "info" and is stored as a human
readable reference string.  Blank lines and lines that start with `--`
are discarded.


This will be discarded.
