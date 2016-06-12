module Opi = Opium.Std

let print_param = Opi.get "/hello/:name" begin fun req ->
  `String ("Hello " ^ Opi.param req "name") |> Opi.respond'
end

let home = Opi.get "/" begin fun _ ->
  `String (Sinsi_view.render_home_page ()) |> Opi.respond'
end

let channels_route = Opi.get "/channels" begin fun _ ->
  `String (Sinsi_view.render_channels_page Sinsi_feeds.channel_store) |> Opi.respond'
end

let feeds_route = Opi.get "/feeds" begin fun _ ->
    `String (Sinsi_feeds.channel_store_to_json Sinsi_feeds.channel_store) |> Opi.respond'
end

let run_server () =
  let port = 4000 in
  Lwt.async(fun () -> Lwt_io.printlf "Running server at http://localhost:%d" port);
  Opi.App.empty
  |> print_param
  |> home
  |> feeds_route
  |> channels_route
  |> Opi.App.port port
  |> Opi.App.run_command

let main () =
  Lwt.return (run_server ())

let _ =
  Lwt.async (Sinsi_feeds.feed_update_worker);
  Lwt_main.run (main ())
