
open Lwt.Infix
open Printf


module type Config = sig
  val params : Jw_client.param list
end


module Make (Client : Jw_client.Client)
            (Conf : Config) =
struct

  type t = Jw_client.video
  type offset = int

  let offset_of_string = int_of_string
  let string_of_offset = string_of_int

  let make_stream ?(offset=0) () =
    let offset = ref offset in
    let videos = ref [] in

    Lwt_stream.from (fun () -> 
      (match !videos with 
        | [] -> 
          let params = Jw_client.merge_params
            Conf.params
            ["result_offset", [!offset |> string_of_int]] in
          Client.get_videos_list ~params () >>= fun body ->
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