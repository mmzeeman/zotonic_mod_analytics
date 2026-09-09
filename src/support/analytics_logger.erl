do_flush_copy(Conn, Rows) ->
    Schema = z_config:get(analytics_db_schema, "public"),
    SchemaTable = Schema ++ ".access_log",
    
    %% Start COPY command
    CopySQL = <<"COPY ", (iolist_to_binary(SchemaTable))/binary, " (
                    req_version, req_method, req_bytes,
                    resp_category, resp_code, resp_bytes,
                    site, path, qs, referer,
                    controller, dispatch_rule, rsc_id,
                    duration_process, duration_total,
                    peer_ip, session_id, user_id,
                    language, timezone, user_agent, timestamp
                ) FROM STDIN (FORMAT CSV, DELIMITER E'\t', NULL 'null')">>,
    
    case epgsql:squery(Conn, CopySQL) of
        {copy, _} ->
            %% Stream the rows using epgsql's copy protocol
            case stream_copy_rows(Conn, Rows) of
                ok ->
                    %% End COPY with successful marker
                    case epgsql:put_copy_end(Conn) of
                        ok -> ok;
                        {error, _} = Error -> Error
                    end;
                Error ->
                    %% Abort copy on error
                    catch epgsql:put_copy_end(Conn, error),
                    Error
            end;
        SQueryError ->
            SQueryError
    end.
