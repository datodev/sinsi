open Tyxml
module Ptime = Ptime

let%html feed_form = "<form><input type='text' placeholder='Add RSS feed' /></form>"

let reload_script = "const timer = window.setTimeout(() => window.location.reload(), 1000); const clearRefresh = () => { console.log('clear timeout'); window.clearTimeout(timer); document.removeEventListener('mousemove', clearRefresh) }; document.addEventListener('mousemove', clearRefresh);"

let home_page_doc =
  [%html
      {|<html>
       <head>
       <title>Sinsi</title>
       </head>
       <body>|}[feed_form]{|
                           </body>
                           </html>|}
  ]

let option_exn opt =
  match opt with
  | None -> raise Not_found
  | Some value -> value

let pretty_pub_date (pubdate) =
  match pubdate with
  | Some d ->
     let time = option_exn (Ptime.of_float_s d) in
     let tz_offset_s = Ptime_clock.current_tz_offset_s () in
     let b = Buffer.create 17 in
     let fmt = Format.formatter_of_buffer b in
     Format.fprintf fmt "%a@." (Ptime.pp_human ?tz_offset_s ()) time;
     Buffer.contents b
  | None -> ""

let entry_body(entry : Sinsi_feeds.entry) =
  match entry.Sinsi_feeds.body with
  | Some "" | None -> Sinsi_feeds.string_of_option entry.Sinsi_feeds.description
  | Some b -> b

let entry_class (entry) =
  (match entry.Sinsi_feeds.read with | None -> "entry unread" | Some x -> if x then "entry read" else "entry unread")

let mark_as_read (entry_id) =
  "(() => mark_as_read(this,'" ^ entry_id ^ "'))()"

let render_entry (entry : Sinsi_feeds.entry) =
  [%html {|
     <div class=|} [(entry_class entry)] {| onclick='|}(mark_as_read entry.Sinsi_feeds.id){|'>
        <div class='entry-title'>
           |} [Html.Unsafe.data (Sinsi_feeds.string_of_option entry.Sinsi_feeds.title)] {|
        </div>
        <div class='entry-meta'>
          <div class='entry-author'>
             |} [Html.pcdata (Sinsi_feeds.string_of_option entry.Sinsi_feeds.author)] {|
          </div>
          <div class='entry-pubdate'>
             |} [Html.pcdata (pretty_pub_date entry.Sinsi_feeds.pubdate)] {|
          </div>
        </div>
        <div class='entry-body'>
           |} [Html.Unsafe.data (entry_body entry)] {|
        </div>
      </div>

  |}]

let render_entries entries =
  [%html {|<div class='entries-container'>|}
         (List.map render_entry entries)
         {|</div>|}]

let feeds =
  List.map Uri.of_string ["http://gun-moll.herokuapp.com/threads?id=dang";
                          "http://gun-moll.herokuapp.com/threads?id=patio11";
                          "http://gun-moll.herokuapp.com/threads?id=sbensu";
                          "http://hyegar.com/atom.xml";
                         ]

let functions =
  "
    const mark_as_read = (el, entry_id) => {
      const r = new XMLHttpRequest();
      r.open('PUT', '/entry/' + encodeURIComponent(entry_id) + '/mark-read');
      r.send();
      el.className = 'entry read';
    };
  "


let channels_doc (entries) =
  [%html
      {|<html>
       <head>
       <meta charset='utf-8'>
       <link rel='stylesheet' href='/css/sinsi.css' />
       <title>Sinsi</title>
       <script>|}(Html.Unsafe.data reload_script){|</script>
       <script>|}(Html.Unsafe.data functions){|</script>
       </head>
       <body>|}
      [(render_entries entries)]
  {|
     </body>
     </html>
    |}
  ]

let all_entries (channel_store) =
  let open Lwt.Infix in
  let channels = Lwt_list.map_p (fun uri -> Sinsi_feeds.channel_by_uri channel_store uri) feeds in
  let res = channels >>= (fun chs ->
      Lwt.return (
        List.fold_left (fun acc ch ->
            (List.append acc (match ch with
                | None -> []
                | Some c -> c.Sinsi_feeds.entries)))
          []
          chs)) in
  res

let render_channels_page (channel_store) =
  let b = Buffer.create 17 in
  let fmt = Format.formatter_of_buffer b in
  let open Lwt.Infix in
  (all_entries channel_store) >>= (fun entries ->
      Html.pp () fmt (channels_doc entries);
      Lwt.return (Buffer.contents b)
    )

let render_home_page () =
  let b = Buffer.create 17 in
  let fmt = Format.formatter_of_buffer b in
  Html.pp () fmt home_page_doc;
  Lwt.return (Buffer.contents b)
