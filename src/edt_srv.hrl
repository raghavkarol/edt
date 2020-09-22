-ifndef(__edt_srv__).
-define(__edt_srv__, 1).

-record(change,
        {action   :: 'compile'|'reload',
         path     :: binary(),
         count=0  :: integer()}).

-endif.
