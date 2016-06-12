open Tyxml

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

let entry (entry : Sinsi_feeds.entry) =
  [%html {|<div class='entry'>
                      <div class='title'>
                                |} [Html.pcdata(Sinsi_feeds.string_of_option entry.Sinsi_feeds.title)]
         {|
          </div>
          </div>
          |}]

let entries entries =
  [%html {|<div class='entries-container'>|}
         (List.map entry entries)
         {|</div>|}]

let all_entries (channel_store : Sinsi_feeds.channel_store_type) =
  Hashtbl.fold (fun _k v acc ->
                List.append acc v.Sinsi_feeds.entries)
               channel_store
               []

let channels_doc (channel_store : Sinsi_feeds.channel_store_type) =
  [%html
      {|<html>
       <head>
       <title>Sinsi</title>
       <script>|}(Html.Unsafe.data reload_script){|</script>
       </head>
       <body>|}
      [(entries (all_entries channel_store))]
  {|
     </body>
     </html>
    |}
  ]

let render_channels_page (channel_store : Sinsi_feeds.channel_store_type) =
  let b = Buffer.create 17 in
  let fmt = Format.formatter_of_buffer b in
  Html.pp () fmt (channels_doc channel_store);
  Buffer.contents b

let render_home_page () =
  let b = Buffer.create 17 in
  let fmt = Format.formatter_of_buffer b in
  Html.pp () fmt home_page_doc;
  Buffer.contents b
