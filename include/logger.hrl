-compile([{parse_transform, lager_transform}]).

-define(log(Level, Meta, Fmt, Args), lager:Level(Meta, Fmt, Args)).
-define(log(Level, Fmt, Args), lager:Level(Fmt, Args)).
-define(log(Level, Fmt), lager:Level(Fmt)).

-define(debug(Fmt), ?log(debug, Fmt)).
-define(info(Fmt), ?log(info, Fmt)).
-define(notice(Fmt), ?log(notice, Fmt)).
-define(warning(Fmt), ?log(warning, Fmt)).
-define(error(Fmt), ?log(error, Fmt)).
-define(critical(Fmt), ?log(critical, Fmt)).
-define(alert(Fmt), ?log(alert, Fmt)).
-define(emergency(Fmt), ?log(emergency, Fmt)).
-define(none(Fmt), ?log(none, Fmt)).

-define(debug(Fmt, Args), ?log(debug, Fmt, Args)).
-define(info(Fmt, Args), ?log(info, Fmt, Args)).
-define(notice(Fmt, Args), ?log(notice, Fmt, Args)).
-define(warning(Fmt, Args), ?log(warning, Fmt, Args)).
-define(error(Fmt, Args), ?log(error, Fmt, Args)).
-define(critical(Fmt, Args), ?log(critical, Fmt, Args)).
-define(alert(Fmt, Args), ?log(alert, Fmt, Args)).
-define(emergency(Fmt, Args), ?log(emergency, Fmt, Args)).
-define(none(Fmt, Args), ?log(none, Fmt, Args)).

-define(debug(Meta, Fmt, Args), ?log(debug, Meta, Fmt, Args)).
-define(info(Meta, Fmt, Args), ?log(info, Meta, Fmt, Args)).
-define(notice(Meta, Fmt, Args), ?log(notice, Meta, Fmt, Args)).
-define(warning(Meta, Fmt, Args), ?log(warning, Meta, Fmt, Args)).
-define(error(Meta, Fmt, Args), ?log(error, Meta, Fmt, Args)).
-define(critical(Meta, Fmt, Args), ?log(critical, Meta, Fmt, Args)).
-define(alert(Meta, Fmt, Args), ?log(alert, Meta, Fmt, Args)).
-define(emergency(Meta, Fmt, Args), ?log(emergency, Meta, Fmt, Args)).
-define(none(Meta, Fmt, Args), ?log(none, Meta, Fmt, Args)).