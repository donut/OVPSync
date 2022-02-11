
open Base

module type DBC = Caqti_lwt.CONNECTION

open Lwt.Infix
open Lib.Infix.Function


module Util = struct
  let use_pool p f =
    let%lwt result = p |> Caqti_lwt.Pool.use begin fun dbc ->
      match%lwt f dbc with
      | exception exn -> Lwt.return @@ Ok (Error exn)
      | x -> Lwt.return @@ Ok (Ok x)
    end >>= Caqti_lwt.or_fail in

    match result with
    | Error exn -> raise exn
    | Ok x -> Lwt.return x


  let run p_or_c f =
    match p_or_c with
    | `Connection c -> f c
    | `Pool p -> use_pool p f


  let exec porc query values = run porc <| fun (module DB : DBC) ->  
    DB.exec query values >>= Caqti_lwt.or_fail

  let collect_list porc query values = run porc <| fun (module DB : DBC) -> 
    DB.collect_list query values >>= Caqti_lwt.or_fail
end


type video =
  { video_id : int
  ; title : string
  ; canonical : string option
  ; duplicates : string list }


module Q = struct
  module Creq = Caqti_request
  open Caqti_type

  let select_duplicate_sources = Creq.collect
    (tup2 string string) (tup4 int string (option string) string)
    "SELECT video.id \
          , video.title \
          , canon.media_id \
          , GROUP_CONCAT(src.media_id ORDER BY src.id SEPARATOR ';') \
       FROM video \
       JOIN source AS src ON src.video_id = video.id \
  LEFT JOIN source AS canon \
         ON canon.id = video.canonical_source_id  \
        AND canon.name = ? \
      WHERE src.name = ? \
   GROUP BY video.id \
     HAVING COUNT(src.id) > 1"


  let update_canonical_by_media_id = Creq.exec
    (tup3 string string int)
    "UPDATE video \
        SET canonical_source_id = \
              (SELECT video_id FROM source \
                WHERE name = ? AND media_id = ? \
                LIMIT 1) \
      WHERE id = ? \
      LIMIT 1"


  let delete_source_by_media_id = Creq.exec
    (tup2 string string)
    "DELETE FROM source WHERE name = ? AND media_id = ? LIMIT 1"
end


let select_duplicate_sources dbc ~source_name =
  Util.collect_list dbc Q.select_duplicate_sources (source_name, source_name)

  >|= List.map ~f:begin fun (video_id, title, canonical, sources) ->
    let duplicates = sources |> String.split ~on:';' in
    { video_id; title; canonical; duplicates }
  end


let update_canonical_by_media_id dbc ~video_id ~source_name ~media_id =
  Util.exec dbc Q.update_canonical_by_media_id (source_name, media_id, video_id)


let delete_source_by_media_id dbc ~source_name ~media_id =
  Util.exec dbc Q.delete_source_by_media_id (source_name, media_id)