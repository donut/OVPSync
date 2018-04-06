
open Lwt.Infix
open Printf


module type Config = sig
  val params : Jw_client.param list
end


module Make (Client : Jw_client.Client)
            (Log : Sync.Logger)
            (Conf : Config) =
struct

  type t = Jw_client.video
  type offset = int

  let offset_of_string = int_of_string
  let string_of_offset = string_of_int

  let make_stream ?(offset=0) () =
    (* 1: Check if video is published and has passthrough *)
    (* 2: If not, add to runtime and permanent list for revisiting and clean up
          later and make API calls to publish and/or add passthrough. *)
    (* 3: Loop through current set all have been passed on to dest. *)
    (* 4: Clean up those confirmed synced to destination, via 
          [cleanup t -> unit Lwt.t] *)
    (* 6: At the end of each set, check permanent list to clean up any times
          not in current list. *)
    (* 7: Move to next set. *)
    let offset = ref offset in
    let videos = ref [] in

    Lwt_stream.from (fun () -> 
      (match !videos with 
        | [] -> 
          Log.info "List empty, getting more from JW" >>= fun () ->
          let params = Jw_client.merge_params
            Conf.params
            ["result_offset", [!offset |> string_of_int]] in
          Client.get_videos_list ~params () >>= fun body ->
          Log.info "Got next set of videos from JW" >>= fun () ->
          offset := body.offset;
          Lwt.return body.videos

        | l -> Lwt.return l
      )
      >>= function
      | [] ->
        Log.info "Completed streaming all JW videos." >>= fun () ->
        Lwt.return None
      | h :: tl ->
        Log.info "Got video" >>= fun () ->
        videos := tl;
        offset := !offset + 1;
        Lwt.return @@ Some (!offset, h)
    )


  let cleanup t = Lwt.return ()

end