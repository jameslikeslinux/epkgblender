% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (epkgblender_table).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("epkgblender.hrl").
-compile(export_all).

reflect() -> record_info(fields, epkgblender_table).

render_element(Record) -> 

    Header = case Record#epkgblender_table.header of
      [] -> "";
      _ -> wf_tags:emit_tag(thead, Record#epkgblender_table.header, [])
    end,

    Footer = case Record#epkgblender_table.footer of
      [] -> "";
      _ -> wf_tags:emit_tag(tfoot, Record#epkgblender_table.footer, [])
    end,

    Body = wf_tags:emit_tag(tbody, Record#epkgblender_table.rows, []),
    Content = [Header, Footer, Body ],

    wf_tags:emit_tag( table, Content, [
        {class, [table, Record#epkgblender_table.class]},
        {style, Record#epkgblender_table.style}
    ]).
