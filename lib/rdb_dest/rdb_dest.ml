
module Client = Client
module Video = Video

open Lwt.Infix


module Make (DB : Caqti_lwt.CONNECTION) = struct

  type t = Video.t
  
  let save t =
    (* 1: Check if a video with one of the same ovp+media exists. *)
    (* 2a: If exists, update *)
    (* 2b: If not, save new video *)
    (* 3: Update existing sources *)
    (* 4: Save new sources *)
    (* 5: Update canonical source if changed *)
    (* 6: Save custom data *)
    (* 7: Save tags *)
    (* 8: [jwsrc] Try getting feed *)
    (* 9a: [jwsrc] If exists, set file and thumb URIs *)
    (* 9b: [jwsrc] If non-existent, guess at file and thumb URIs? or set None *)
    (* 10: Use MD5 to see if existing file needs updating
     *     Be sure JW updates the MD5 when the original video file is
     *     replaced. *)
    (* 11: Prepare directory to save files *)
    (* 12: Save video file if new/changed *)
    (* 14: Delete old video file if it wasn't replaced *)
    (* 15: Save thumbnail file if new/changed *)
    (* 16: Delete old thumbnail file if it wasn't replaced *)
    Lwt.return t

end