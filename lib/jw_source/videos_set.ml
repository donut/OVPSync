
open Base
open Lwt.Infix
open Lib.Lwt_result.Just_let_syntax
open Lib.Infix.Function

module Lwt_result = Lib.Lwt_result

module type Jw_platform = Jw_client.Platform.Client
module type VS = Sync.Variable_store

type videos_video = Jw_client.Platform.videos_list_video


let var_store_offset_key = "request_offset"


type t =
  { all : videos_video list
    (** All videos in current set. *)
  ; to_check : videos_video list
    (** Videos that have not been checked yet. *)
  ; processing : videos_video list
    (** Videos that are being externally processed and need to be checked again
        after exhausting [to_check]. *)
  }


let empty () = { all = []; to_check = []; processing = [] }


type step =
  | All_sets_finished
  | New_set of ([`Offset of int] * [`Count of int])
  | Set_finished
  | New_in_current_set of int
  | Processing_to_check of int
  | Next of videos_video


let video_key (v : videos_video) = v.key


let select_videos ~from:la ~not_in:lb =
  la
  |> List.filter ~f:begin fun a ->
    lb
    |> List.find ~f:(video_key %> String.equal (video_key a))
    |> Option.is_none
  end


module type Config = sig
  val base_jw_request_params : Jw_client.Platform.param list
end


module Make = functor
  (Platform : Jw_client.Platform.Client)
  (Var_store : Sync.Variable_store)
  (Config : Config)
-> struct

  let current = ref (empty ())


  let current_offset () =
    Var_store.get var_store_offset_key ~default:"0" ()
    >|= Int.of_string


  let increment_offset by =
    let%lwt new_offset = current_offset () >|= (+) by in

    let%lwt () =
      new_offset
      |> Int.to_string
      |> Var_store.set var_store_offset_key
    in

    Lwt.return new_offset


  let clear_offset () =
    Var_store.delete var_store_offset_key


  let get ~params ~offset =
    let params = Jw_client.Util.merge_params
      params
      [ "result_offset", [offset |> Int.to_string] ]
    in

    let%bind { videos; _ } = Platform.videos_list ~params () in
    Lwt_result.return videos


  let videos_at_current_offset () =
    let%lwt offset = current_offset () in
    get ~params:Config.base_jw_request_params ~offset


  let step () =
    match !current with 
    (* No videos loaded from current offset, yet. *)
    | { all = []; _ } ->
      let%lwt offset = current_offset () in
      let%bind all = get ~params:Config.base_jw_request_params ~offset in

      begin match all with 
      | [] ->
        (* No more videos to process. *)
        let%lwt () = clear_offset () in
        Lwt_result.return
          All_sets_finished

      | _ ->
        let () = current := { all; to_check = all; processing = [] } in
        Lwt_result.return @@
          New_set (`Offset offset, `Count (List.length all))
      end

    (* All videos checked and processed. *)
    | { all; to_check = []; processing = [] } ->
      (* Check for new videos at the current offset in case more were 
        added (or some removed, pushing more into the current offset) in 
        the time it took to process the current set *)
      let%bind latest_at_offset = videos_at_current_offset () in

      let new_videos_not_in_all =
        select_videos ~from:latest_at_offset ~not_in:all in

      begin match new_videos_not_in_all with
      | [] ->
        (* All videos at the current offset have been checked, time to get
          the next set. *)
        let%lwt _ = increment_offset (List.length all) in
        let () = current := empty () in
        Lwt_result.return
          Set_finished

      | to_check ->
        (* Videos were added at the current offset since the last time we
          checked. *)
        let () = current :=
          { all = latest_at_offset; to_check; processing = [] } in
        Lwt_result.return @@
          New_in_current_set (List.length to_check)
      end

    (* All videos checked at least once, but some are processing and need to
       be checked again. *)
    | { all; to_check = []; processing } ->
      let checked = select_videos ~from:all ~not_in:processing in
      let%bind latest_at_offset = videos_at_current_offset () in
      let to_check = select_videos ~from:latest_at_offset ~not_in:checked in

      let () = current := 
        { all = latest_at_offset; to_check; processing = [] } in
      Lwt_result.return @@
        Processing_to_check (List.length to_check)

    (* There are videos ready to be checked. *)
    | { all; to_check = hd :: tl; processing } ->
      let () = current :=
        { all; to_check = tl; processing } in
      Lwt_result.return @@
        Next hd


  let add_to_processing video =
    let { processing; _ } = !current in
    current := { !current with processing = video :: processing } 
end (* struct *)