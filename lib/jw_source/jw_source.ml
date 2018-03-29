
open Lwt.Infix
open Printf

module Make (Client : Jw_client.Client) = struct

  let make_stream ?(params=[]) ?(offset=0) () =
    let offset = ref offset in
    let videos = ref [] in

    Lwt_stream.from (fun () -> 
      (match !videos with 
        | [] -> 
          let params' = Jw_client.merge_params
            params
            ["result_offset", [!offset |> string_of_int]] in
          Client.get_videos_list ~params:params' () >>= fun body ->
          offset := body.offset;
          Lwt.return body.videos

        | l -> Lwt.return l

      ) >>= function
      | [] -> Lwt.return None
      | h :: tl ->
        videos := tl;
        offset := !offset + 1;
        Lwt.return @@ Some (!offset, h)
    )

end