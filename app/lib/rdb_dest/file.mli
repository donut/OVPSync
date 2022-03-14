(** For performing operations and inspections on files, local and remote. *)


exception File_error of string * string * string
exception Request_failure of string * string * exn
exception Timeout of string * string
exception Unexpected_response_status of string * string * string


val md5 : string -> string
(** [md5 path] generates an MD5 for the file at [path]. Raises {!exn} if
    [path] does not exist. @see {!Digest.path}. *)

val sanitize : string -> string
(** [sanitize name] replaces any characters in [name] that may cause issues
    in a file system. Note that [name] is a filename without a path.  *)

val dir_of_timestamp : int -> string
(** [dir_of_timestamp timestamp] generates a path based on [timestamp] like
    "<year>/<month>/<day>". *)

val trim_slashes : string -> string
(** [trim_slashes path] will trim forward slashes from the beginning and end
    of [path]. *)

val prepare_dir : prefix:string -> string -> string Lwt.t
(** [prepare_dir ~prefix path] creates all intermediate directories from
    [prefix] to [path], including [prefix]. *)

val ext : string -> string option
(** [ext filename] returns the extension of [filename] if it can be 
    determined. *) 

val basename : string -> string
(** [basename path] returns the last part of [path] without any preceding
    directories or its final extension. For example, passing in
    "/Users/example/file.bmp.zip" would return "file.bmp". *)

val restrict_name_length : string -> string -> string
(** [restrict_name_length basename ext] reduces the filename represented by 
    [basename] and [ext] to a length that is safe for most operating/file 
    systems if necessary. The returned value is the potentially reduced 
    [basename] with [ext] suffixed to it with a ".". If the filename was 
    shortened, a "---" prefix will have been added to the basename.

    This doesn't account for characters that take up more than 1 byte.
    
    @see https://serverfault.com/a/9548/54523 *)

val content_length_of_uri : Uri.t -> int64 option Lwt.t
(** [content_length_of_uri uri] performs a GET request to [uri] and checks for
    content length headers (@see {!Cohttp.Header.get_content_range}) and returns
    that value if any. The request is cancelled before retreiving the body 
    effectively simulating a HEAD request.
    
    This would have been designed to use a HEAD request, but the URIs I was 
    dealing with when I built this returned "405 Method Not Allowed". *)

val download : Uri.t -> to_:string -> unit Lwt.t 
(** [download uri path] downloads HTTP(S) [uri] to absolute [path]. Will
    follow redirects. *)

val unlink_if_exists : string -> unit Lwt.t
(** [unlink_if_exists path] delets the file at lcoal absolute [path] if it 
    exists. *)