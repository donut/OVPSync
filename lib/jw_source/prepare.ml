

open Base
open Lwt.Infix
open Lib.Lwt_result.Just_let_syntax
open Lib.Infix.Function
open Lib.Infix.Option

module Lwt_result = Lib.Lwt_result


type accounts_template = Jw_client.Platform.accounts_templates_list_template
type videos_video = Jw_client.Platform.videos_list_video
type videos_conversion = Jw_client.Platform.videos_conversions_list_conversion

type t = videos_video
        * (string * int option * int option) option
        * string option


let original_thumb_url media_id = 
  Printf.sprintf "https://cdn.jwplayer.com/thumbs/%s.jpg" media_id


let get_status_and_passthrough media_id =
  let params = [("cache_break", [Random.bits () |> Int.to_string])] in

  match%map Jw_client.Delivery.get_media media_id ~params () with
  | None ->
    (false, None)

  | Some { playlist = []; _ } ->
    (true, None)

  | Some { playlist = media :: _; _ } -> 
    let passthrough =
      media.sources
      |> List.find ~f:begin fun s -> 
        match Jw_client.V2_media_body_t.(s.label) with
        | None -> false
        | Some l -> String.(equal (lowercase l) "passthrough")
      end
    in
    (true, passthrough)


type prepared = 
  | Has_non_ready_status
  | Source_is_URL
  | Published_with_passthrough


type status =
  | No_need_to_sync
  | Missing
  | Processing
  | Prepared of t * prepared


module type Config = sig
  val backup_expires_field : string
  val temp_pub_tag : string
end


module Make = functor
  (Platform : Jw_client.Platform.Client)
  (Var_store : Sync.Variable_store)
  (Log : Logger.Sig) 
  (Config : Config)
-> struct
  let var_store = (module Var_store : Sync.Variable_store)


  let undo_publish expires ({ key; expires_date; tags; _ } : videos_video) =
    (* Make sure we have the latest expires date in case it was set
      outside of this program. *)
    let expires = expires_date =?: expires in

    let tags = tags
      |> String.split_on_chars ~on:[',']
      |> List.map ~f:(String.strip ?drop:None)
      |> List.filter ~f:(String.equal Config.temp_pub_tag %> not)
      |> String.concat ~sep:", "
    in

    (* "-" prefix tells JW to remove the custom field *)
    let backup_expires_field = "custom.-" ^ Config.backup_expires_field in

    let params =
      [ ("expires_date", [expires |> Int.to_string])
      ; (backup_expires_field, [""])
      ; ("tags", [tags]) ]
    in

    Platform.videos_update key params


  let remove_added_passthrough media_id = 
      match%lwt Platform.delete_conversion_by_name media_id "passthrough" with
      | Error (Not_found_s _) (* Likely deleted outside this program. *)
      | Ok () ->
        let%lwt () = Changes.clear_record var_store media_id in
        Lwt_result.return ()

      | Error e ->
        Lwt_result.fail e


  let cleanup_by_media_id ?changed media_id =
    let%lwt () = Log.debugf "Undoing changes to [%s]." media_id in

    let%lwt { expires; passthrough; _ } =
      match changed with 
      | None -> Changes.get_record var_store media_id
      | Some c -> Lwt.return c 
    in

    let%bind_open () =
      match expires with 
      | None -> return ()

      | Some expires_date ->
        let%lwt () = Log.debugf "[%s] Undoing publish." media_id in

        match%bind Platform.videos_show media_id with
        | None -> return ()
        | Some { video; _ } -> undo_publish expires_date video
    in
    
    let%bind_open () = 
      if not passthrough then return ()
      else

      let%lwt () = Log.debugf
        "[%s] Deleting passthrough conversion." media_id in
      remove_added_passthrough media_id
    in

    let%lwt () =
      if passthrough || Option.is_some expires
      then Log.infof "[%s] Undid all changes." media_id
      else Log.debugf "[%s] No changes to undo." media_id
    in

    Lwt_result.return ()


  let cleanup_old_changes ~exclude ?(min_age=0) () = 
    Changes.get_all_records var_store ~except:exclude ~min_age ()

    >>= Lwt_list.iter_p begin fun (media_id, changed) ->
      match%lwt cleanup_by_media_id ~changed media_id with
      | Error exn ->
        let%lwt () = Log.errorf ~exn "[%s] Failed cleaning up." media_id in
        Lwt.return ()

      | Ok () ->
        Lwt.return ()
    end

    
  let publish_video ({ key; tags; expires_date; _ } : videos_video) =
    let tags =
      tags
      |> String.split_on_chars ~on:[',']
      |> List.map ~f:(String.strip ?drop:None)
      |> (fun l -> Config.temp_pub_tag :: l)
      |> String.concat ~sep:", "
    in

    let backup_expires_field = "custom." ^ Config.backup_expires_field in
    let expires_date = expires_date >|? Int.to_string =?: "" in

    let params =
      [ ("expires_date", [""])
      ; (backup_expires_field, [expires_date])
      ; ("tags", [tags]) ]
    in

    Platform.videos_update key params

  
  let publish_video_if_needed
    ({ key; expires_date; _ } as vid : videos_video)
    ~(changes : Changes.Record.t)
    ~published
  = 
    let now = Unix.time () |> Int.of_float in

    match published, expires_date with
    | true, _ ->
      Lwt_result.return changes

    | false, None ->
      let%lwt () = Log.infof "[%s] Waiting on publish." key in
      Lwt_result.return changes

    | false, Some e when e > now ->
      let%lwt () = Log.infof "[%s] Waiting on publish." key in
      Lwt_result.return changes

    | false, Some _ ->
      let%lwt () = Log.infof "[%s] Not published; publishing..." key in

      let changes = { changes with expires = expires_date } in
      let%lwt () = Changes.set_record var_store key changes in
      let%bind () = publish_video vid in

      Lwt_result.return changes


  (* Cache the passthrough template key. This will save nearly 1 API request
     per video processed. *)
  let cached_passthrough_template_key = ref None

  let passthrough_template_key () =
    match !cached_passthrough_template_key with
    | Some k ->
      Lwt_result.return k

    | None ->
      let%bind body = Platform.accounts_templates_list () in

      body.templates

      |> List.find ~f:begin fun (t : accounts_template) ->
        String.(equal (lowercase t.name) "passthrough")
      end

      |> function
        | None -> 
          Lwt_result.fail @@ Not_found_s
            (Parsexp.Single.parse_string_exn "passthrough template not found.")

        | Some { key; _ } ->
          let () = cached_passthrough_template_key := Some key in
          Lwt_result.return key



  let add_passthrough_conversion media_id =
    let%bind key = passthrough_template_key () in
    Platform.videos_conversions_create media_id key


  let add_passthrough_if_needed 
    ({ key; _ } : videos_video)
    ~(changes : Changes.Record.t)
    ~passthrough
  =
    if Option.is_some passthrough then Lwt_result.return changes
    else

    let%bind_open needs_passthrough = 
      if not changes.passthrough then return true
      else

      let%bind { conversions; _ } = Platform.videos_conversions_list key in

      let passthrough =
        conversions
        |> List.find ~f:begin fun (c : videos_conversion) ->
          String.(equal (lowercase c.template.name) "passthrough")
        end
      in

      match passthrough with
      | Some { status = `Failed; key = conversion_key; _ } ->
        (* @todo Deal with passthrough conversions that fail every time.
                This has the potential to infinitely loop. *)
        let%lwt () =
          Log.errorf "[%s] Passthrough conversion creation failed." key in
        let%bind () = Platform.videos_conversions_delete conversion_key in
        return true

      | Some _ | None ->
        return false
    in

    if needs_passthrough then
      let%lwt () = Log.infof "[%s] No passthrough; creating..." key in

      let changes = { changes with passthrough = true } in
      let%lwt () = Changes.set_record var_store key changes in
      let%bind () = add_passthrough_conversion key in

      Lwt_result.return changes

    else
      let%lwt () = Log.infof "[%s] Waiting on passthrough." key in
      Lwt_result.return changes


  (** [clear_temp_changes_for_return ?changed vid] Makes sure any changes to
      [vid] that are just to facilitate the sync process are not actually
      synced to the destination. *)
  let clear_temp_changes_for_return ?changed (vid : videos_video) =
    let%lwt { expires; _ } =
      match changed with 
      | None -> Changes.get_record var_store vid.key
      | Some c -> Lwt.return c 
    in
    
    if Option.is_none expires then Lwt.return vid
    else

    let tags =
      vid.tags
      |> String.split_on_chars ~on:[',']
      |> List.filter ~f:(String.equal Config.temp_pub_tag %> not)
      |> String.concat ~sep:", "
    in   

    let custom = 
      vid.custom
      |> List.filter ~f:((fst %> String.equal Config.backup_expires_field)) 
    in

    Lwt.return { vid with expires_date=expires; tags; custom }


  let video
      ({ key; status; sourcetype; sourceurl; _ } as vid : videos_video)
      ~(should_sync : t -> bool Lwt.t) 
  =
    let%lwt sync_needed = 
      let%lwt vid = clear_temp_changes_for_return vid in
      should_sync (vid, None, None)
    in

    if not sync_needed then 
      let%bind () = cleanup_by_media_id key in
      Lwt_result.return No_need_to_sync
    else

    match status, sourcetype with
    | _, `URL
    | (`Created | `Processing | `Updating | `Failed), `File ->
      (* Since this program is designed to be run over and over again, 
          constantly syncing media from JW, we can catch anything that's
          processing the next time we reach this offset. *)
      let%lwt vid = clear_temp_changes_for_return vid in
      let file = sourceurl >|? (fun s -> (s, None, None)) in
      let thumb = original_thumb_url key in
      let t = (vid, file, Some thumb) in

      Lwt_result.return begin
        match sourcetype with
        | `File -> Prepared (t, Has_non_ready_status)
        | `URL -> Prepared (t, Source_is_URL)
      end

    | `Ready, `File ->
      let%lwt () = Log.debugf
        "[%s] Getting publish and passthrough status." key in

      begin match%bind get_status_and_passthrough key with
      | (false as published), passthrough
      | published, (None as passthrough) ->
        let changes = 
          let%lwt c = Changes.get_record var_store key in
          let%bind c = publish_video_if_needed ~changes:c ~published vid in
          let%bind c = add_passthrough_if_needed ~changes:c ~passthrough vid in
          Lwt_result.return c
        in

        begin match%lwt changes with
        | Error (Not_found_s _) -> Lwt_result.return Missing
        | Error e -> Lwt_result.fail e
        | Ok _ -> Lwt_result.return Processing
        end

      | true, Some { file; width; height; _ } ->
        let%lwt vid = clear_temp_changes_for_return vid in
        let thumb = original_thumb_url key in
        let t = (vid, Some (file, width, height), Some thumb) in

        Lwt_result.return @@
          Prepared (t, Published_with_passthrough)
      end
end