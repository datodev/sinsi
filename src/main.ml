module Opi = Opium.Std

module Rss = Syndic.Rss2

let string_of_option = function
  | None -> ""
  | Some s -> s

let string_of_option_uri = function
  | None -> ""
  | Some uri -> Uri.to_string uri

let item_title (item : Rss.item) =
  let open Syndic.Rss2 in
  match item.story with
  | All (s, _, _) -> Some s
  | Title s -> Some s
  | Description (_, _) -> Some ""

let item_description (item : Rss.item) =
  let open Syndic.Rss2 in
  match item.story with
  | All (_, _, d) -> Some d
  | Description (_, d) -> Some d
  | Title _ -> None

type entry =
  {
    title: string option;
    guid: string option;
    link: string option;
    author: string option;
    pubdate: float option;
    body: string option;
    description: string option;
  } [@@deriving yojson]

type channel =
  {
    title: string option;
    description: string option;
    link: string option;
    html_uri: string option;
    entries: entry list;
  } [@@deriving yojson]

let entry_text (entry : entry) =
  match entry.body with
  | Some b -> b
  | None -> match entry.description with
    | None -> ""
    | Some d -> d

let rss_item_to_entry (item : Rss.item) =
  let open Syndic.Rss2 in
  let title       = item_title item in
  let guid        = match item.guid with
    | None -> None
    | Some t -> Some (Uri.to_string t.data) in
  let link        = Some (string_of_option_uri item.link) in
  let author      = item.author in
  let pubdate     = match item.pubDate with
    | None -> None
    | Some time -> Some (Ptime.to_float_s time) in
  let (_, body)        = item.content in
  let description = item_description item in
  {
    title       = title;
    guid        = guid;
    link        = link;
    author      = author;
    pubdate     = pubdate;
    body        = Some body;
    description = description;
  }

let feeds =
  List.map Uri.of_string ["http://gun-moll.herokuapp.com/threads?id=dang"]

let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String ("Super Live(ish) reload!") |> Opi.respond'
end

let run_server () =
  Opi.App.empty
  |> print_param
  |> home
  |> Opi.App.port 4000
  |> Opi.App.run_command

let fetch_feed uri =
  let open Lwt.Infix in
  let module Client = Cohttp_lwt_unix.Client in
  Client.get uri >>= fun (_resp, body) ->
  Cohttp_lwt_body.to_string body >|= fun body ->
  Printf.printf "Uri %s has body of length: %d\n" (Uri.to_string uri) (String.length body);
  body

let fetch_and_print_feeds () =
  let open Lwt.Infix in
  let open Syndic.Rss2 in
  print_endline "Getting feed bodies";
  let feed_bodies = List.map fetch_feed feeds in
  print_endline "Iterating over feed bodies";
  Lwt_list.iter_s (fun _body ->
      print_endline "Feed body loop";
      _body >>= fun body ->
      let xml_source = Xmlm.make_input (`String (0, body)) in
      let rss = Rss.parse xml_source in
      let items = rss.items in
      let entries = List.map rss_item_to_entry items in
      let channel =
        {
          title = None;
          description = None;
          link = None;
          html_uri = None;
          entries = entries
        }
      in
      let yojson = channel_to_yojson channel in
      let json = Yojson.Safe.to_string yojson in
      Printf.printf "JSON: %s \n" json;
    Lwt.return_unit) feed_bodies

let main () =
  print_endline "Fetching feed...";
  (* Lwt.return (run_server ()) *)
  fetch_and_print_feeds ()

let _ =
  Lwt_main.run (main ())
