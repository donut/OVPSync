
open Base

module Result_lwt = Lib.Result_lwt
open Lib.Infix.Option


exception Caqti_conn_error of string
exception Missing_argument of string
exception Extra_arguments of string list


let db_conn, jw_key, jw_secret =
  match Sys.argv |> Array.to_list with
  | [] | [_] ->
    raise @@ Missing_argument "Database connection string"
  | [_; _db_conn] ->
    raise @@ Missing_argument "JW API key"
  | [_; _db_conn; _jw_key] ->
    raise @@ Missing_argument "JW API secret"
  | [_; db_conn; jw_key; jw_secret] ->
    (db_conn, jw_key, jw_secret)
  | _ :: _ :: _ :: _ :: extras ->
    raise @@ Extra_arguments extras


let dbc =
  match Caqti_lwt.connect_pool (Uri.of_string db_conn) with
  | Ok p -> `Pool p
  | Error e -> raise @@ Caqti_conn_error (Caqti_error.show e)


module Log = Logger.Make(struct
  let prefix = ""
  let level = `Trace
end) 


module Platform = Jw_client.Platform.Make(Log)(struct
  let key = jw_key
  let secret = jw_secret
  let rate_limit_to_leave = 5
end)


let source_name = "jw-" ^ jw_key


let dedupe ({ video_id; title; canonical; duplicates = dupes } : Db.video) =
  let%lwt () = Log.infof "[%d] %s" video_id title in
  let%lwt () = Log.infof "--> Canonical: %s" (canonical =?: "[None]") in
  let%lwt () = Log.infof "--> Duplicates: %s" (String.concat ~sep:"; " dupes) in

  let%lwt exist, missing = dupes |> Lwt_list.partition_s begin fun id ->
    (* Platform is used over Delivery API in case video was unpublished, in
       which case Delivery would return 404 but Platform would reutrn properly.
       *)
    match%lwt Platform.videos_show id with
    | Error exn ->
      let%lwt () = Log.errorf ~exn "--> [GET /videos/show] for %s failed." id in
      Lwt.return true (* Assume it exists. *)

    | Ok Some _ ->
      Lwt.return true
    
    | Ok None ->
      let%lwt () = Log.infof "--> %s missing at JW." id in
      Lwt.return false
  end in

  let keeper, dupes, missing =
    match exist with
    | [] ->
      let keeper, missing = List.(hd_exn dupes, tl dupes =?: []) in
      keeper, [], missing

    | keeper :: dupes ->
      keeper, dupes, missing
  in

  let%lwt () = Log.infof "--> Keeper: %s" keeper in

  let%lwt () =
    match canonical with
    | None ->
      Lwt.return ()
    | Some media_id when String.equal media_id keeper ->
      Lwt.return ()
    | Some old ->
      let%lwt () = Log.infof 
        "--> Setting canonical source to keeper from %s." old in 
      (* Db.update_canonical_source_by_media_id
        dbc ~video_id ~source_name ~media_id:keeper *)
      Lwt.return ()
  in

  let%lwt () = missing |> Lwt_list.iter_s begin fun media_id ->
    let%lwt () = Log.infof
      "--> Deleting missing source %s from local DB." media_id in
    Db.delete_source_by_media_id dbc ~source_name ~media_id
  end in

  let%lwt () = dupes |> Lwt_list.iter_s begin fun media_id ->
    let%lwt () = Log.infof
      "--> Deleting duplicate source %s from JW." media_id in
    let%lwt () = Platform.videos_delete [media_id] |> Result_lwt.to_lwt in

    let%lwt () = Log.infof
      "--> Deleting duplicate source %s from local DB." media_id in
    let%lwt () = Db.delete_source_by_media_id dbc ~source_name ~media_id in
    
    Lwt.return ()
  end in
  
  Lwt.return ()


let main () =
  let%lwt dupes = Db.select_duplicate_sources dbc ~source_name in
  Lwt_list.iter_s dedupe dupes


let () =
  Random.self_init ();
  Lwt_main.run @@ main ()