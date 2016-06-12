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

type channel_store_type = (Uri.t, channel) Hashtbl.t
let channel_store : channel_store_type = (Hashtbl.create 100)

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

type _channel_store_json = (string * channel) list [@@deriving yojson]

let channel_store_to_json (cs : channel_store_type) =
  let inter = Hashtbl.fold (fun uri channel run -> [(Uri.to_string uri, channel)] @ run) cs [] in
  inter
  |> _channel_store_json_to_yojson
  |> Yojson.Safe.pretty_to_string

let feeds =
  List.map Uri.of_string ["http://gun-moll.herokuapp.com/threads?id=dang";
                          "http://gun-moll.herokuapp.com/threads?id=patio11";
                          "http://gun-moll.herokuapp.com/threads?id=sbensu";
                         ]

let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String ("Super Live(ish) reload!") |> Opi.respond'
end

let feeds_route = Opi.get "/feeds" begin fun _ ->
    `String (channel_store_to_json channel_store) |> Opi.respond'
  end

let run_server () =
  let port = 4000 in
  Lwt.async(fun () -> Lwt_io.printlf "Running server at http://localhost:%d" port);
  Opi.App.empty
  |> print_param
  |> home
  |> feeds_route
  |> Opi.App.port port
  |> Opi.App.run_command

let fetch_feed uri =
  let open Lwt.Infix in
  let module Client = Cohttp_lwt_unix.Client in
  Client.get uri >>= fun (_resp, body) ->
  Cohttp_lwt_body.to_string body >|= fun body ->
  Printf.printf "Uri %s has body of length: %d\n" (Uri.to_string uri) (String.length body);
  body

let update_channel_store () =
  print_endline "Fetching feeds";
  let open Lwt.Infix in
  Lwt_list.iter_s (fun uri ->
                   let feed_body = fetch_feed uri in
                   feed_body >>= (fun body ->
                                  let xml_source = Xmlm.make_input (`String (0, body)) in
                                  let rss = Rss.parse xml_source in
                                  let channel = (rss_to_channel uri rss) in
                                  Hashtbl.add channel_store uri channel;
                                  Lwt.return_unit))
                  feeds

let update_feed_interval_in_seconds =
  60 * 60

let rec feed_update_worker () =
  let open Lwt.Infix in
  update_channel_store () >>= fun () ->
  Lwt.async(fun () -> Lwt_io.printlf "Sleeping, %d seconds until next check" update_feed_interval_in_seconds);
  Lwt.bind (Lwt_unix.sleep (float_of_int update_feed_interval_in_seconds))
    (fun () -> feed_update_worker ())

let main () =
  ignore(Lwt.async(update_channel_store));
  Lwt.return (run_server ())

let _ =
  Lwt.async (feed_update_worker);
  Lwt_main.run (main ())
