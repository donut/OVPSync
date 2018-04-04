

type source = {
  name: string;
  id: string;
  created: int;
  updated: int;
  custom: (string * string) list;
}

type t = {
  id: int option;

  title: string;
  slug: string;
  
  created: int;
  updated: int;
  publish: int;
  expires: int option;

  file_uri: Uri.t;
  filename: string;
  md5: string;
  width: int;
  height: int;
  duration: int;
  
  thumbnail_uri: Uri.t;
  description: string;
  tags: string list;
  custom: (string * string) list;

  cms_id: string option;
  link: Uri.t option;

  canonical: source;
  sources: source list;
}