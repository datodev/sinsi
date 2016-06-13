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


let channel_store =
  Sinsi_store.empty Sinsi_store.config

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

let rss_to_channel uri rss =
  let open Syndic.Rss2 in
  let title = Some rss.title in
  let description = Some rss.description in
  let html_uri = Some (Uri.to_string rss.link) in

  let items = rss.items in
  let entries = List.map rss_item_to_entry items in

  {
    title = title;
    description = description;
    link = Some (Uri.to_string uri);
    html_uri = html_uri;
    entries = entries
  }

let feeds =
  List.map Uri.of_string ["http://gun-moll.herokuapp.com/threads?id=dang";
                          "http://gun-moll.herokuapp.com/threads?id=patio11";
                          "http://gun-moll.herokuapp.com/threads?id=sbensu";
                          "http://hyegar.com/atom.xml";
                         ]

let fetch_feed uri =
  let open Lwt.Infix in
  let module Client = Cohttp_lwt_unix.Client in
  Client.get uri >>= fun (_resp, body) ->
  Cohttp_lwt_body.to_string body >|= fun body ->
  Printf.printf "Uri %s has body of length: %d\n" (Uri.to_string uri) (String.length body);
  body

let update_channel_store store =
  print_endline "Fetching feeds";
  let open Lwt.Infix in
  let module Store = Sinsi_store.Store in
  Lwt_list.iter_s (fun uri ->
      let feed_body = fetch_feed uri in
      feed_body >>= (fun body ->
          let xml_source = Xmlm.make_input (`String (0, body)) in
          let rss = Rss.parse xml_source in
          let channel = (rss_to_channel uri rss) in
          let date = Int64.of_int 0 in
          let owner = "Sinsi" in
          let yojson = channel_to_yojson channel in
          let json = Yojson.Safe.pretty_to_string yojson in
          Store.master (Irmin.Task.create ~date ~owner) store >>= fun task ->
          Store.update (task ("Updating " ^ (Uri.to_string uri))) ["channels"; (Uri.to_string uri)] json >>= fun () ->
          Lwt.return_unit))
    feeds

let update_feed_interval_in_seconds =
  60 * 60

let rec feed_update_worker store =
  let open Lwt.Infix in
  update_channel_store store >>= fun () ->
  Lwt.async(fun () -> Lwt_io.printlf "Sleeping, %d seconds until next check" update_feed_interval_in_seconds);
  Lwt.bind (Lwt_unix.sleep (float_of_int update_feed_interval_in_seconds))
    (fun () -> feed_update_worker store)

let channel_by_uri store uri : channel option Lwt.t =
  let open Lwt.Infix in
  let module Store = Sinsi_store.Store in
  let date = Int64.of_int 0 in
  let owner = "Sinsi" in
  Store.master (Irmin.Task.create ~date ~owner) store >>= fun task ->
  Store.read (task ("Reading " ^ (Uri.to_string uri))) ["channels"; (Uri.to_string uri)] >>= function
  | None -> Lwt.return_none
  | Some json ->
    let yojson = Yojson.Safe.from_string json in
    let channel = match channel_of_yojson yojson with
      | `Error _err_str -> None
      | `Ok channel -> Some channel
    in
    Lwt.return channel

let uris store : Uri.t list Lwt.t =
  let open Lwt.Infix in
  let module Store = Sinsi_store.Store in
  Sinsi_store.View.of_path store [] >>= fun value ->
  Sinsi_store.View.list value [] >|= fun key_paths ->
  List.map (function
      | [uri] -> Uri.of_string uri
      | _ -> raise (Failure "Failed listing keys")) key_paths

let print_all_channels feeds store =
  let open Lwt.Infix in
  print_endline "Printing channels!";
  Lwt_list.iter_s (fun uri ->
      channel_by_uri store uri >>=
      (function
        | None -> Lwt.return ()
        | Some channel -> Lwt.return (print_endline (string_of_option channel.title)))) feeds

let rec print_channels_worker feeds store =
  let open Lwt.Infix in
  print_all_channels feeds store >>= fun () ->
  Lwt.async (fun () -> Lwt_io.printlf "Sleeping, %d seconds until next check" update_feed_interval_in_seconds);
  Lwt.bind (Lwt_unix.sleep (float_of_int 2))
    (fun () -> print_channels_worker feeds store)
